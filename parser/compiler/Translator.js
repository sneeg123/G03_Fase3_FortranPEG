import * as CST from "../visitor/CST.js";
import * as Template from "../Templates.js";
import { getActionId, getReturnType, getExprId, getRuleId } from "./utils.js";

/** @typedef {import('../visitor/Visitor.js').default<string>} Visitor */
/** @typedef {import('../visitor/Visitor.js').ActionTypes} ActionTypes*/

/**
 * @implements {Visitor}
 */
export default class FortranTranslator {
  /** @type {ActionTypes} */
  actionReturnTypes;
  /** @type {string[]} */
  actions;
  /** @type {boolean} */
  translatingStart;
  /** @type {string} */
  currentRule;
  /** @type {number} */
  currentChoice;
  /** @type {number} */
  currentExpr;

  /**
   *
   * @param {ActionTypes} returnTypes
   */
  constructor(returnTypes) {
    this.actionReturnTypes = returnTypes;
    this.actions = [];
    this.translatingStart = false;
    this.currentRule = "";
    this.currentChoice = 0;
    this.currentExpr = 0;
  }

  /**
   * @param {CST.Grammar} node
   * @this {Visitor}
   */
  visitGrammar(node) {
    const rules = node.rules.map((rule) => rule.accept(this));

    return Template.main({
      beforeContains: node.globalCode?.before ?? "",
      afterContains: node.globalCode?.after ?? "",
      startingRuleId: getRuleId(node.rules[0].id),
      startingRuleType: getReturnType(
        getActionId(node.rules[0].id, 0),
        this.actionReturnTypes
      ),
      actions: this.actions,
      rules,
    });
  }

  /**
   * @param {CST.Regla} node
   * @this {Visitor}
   */
  visitRegla(node) {
    this.currentRule = node.id;
    this.currentChoice = 0;

    if (node.start) this.translatingStart = true;

    const ruleTranslation = Template.rule({
      id: node.id,
      returnType: getReturnType(
        getActionId(node.id, this.currentChoice),
        this.actionReturnTypes
      ),
      exprDeclarations: node.expr.exprs.flatMap((election, i) =>
        election.exprs
          .filter((expr) => expr instanceof CST.Pluck)
          .map((label, j) => {
            const expr = label.labeledExpr.annotatedExpr.expr;
            return `${expr instanceof CST.Identificador
              ? getReturnType(getActionId(expr.id, i), this.actionReturnTypes)
              : "character(len=:), allocatable"
              } :: expr_${i}_${j}`;
          })
      ),
      expr: node.expr.accept(this),
    });

    this.translatingStart = false;

    return ruleTranslation;
  }

  /**
   * @param {CST.Opciones} node
   * @this {Visitor}
   */
  visitOpciones(node) {
    return Template.election({
      exprs: node.exprs.map((expr) => {
        const translation = expr.accept(this);
        this.currentChoice++;
        return translation;
      }),
    });
  }

  /**
   * @param {CST.Union} node
   * @this {Visitor}
   */
  visitUnion(node) {
    const matchExprs = node.exprs.filter((expr) => expr instanceof CST.Pluck);
    const exprVars = matchExprs.map(
      (_, i) => `expr_${this.currentChoice}_${i}`
    );

    /** @type {string[]} */
    let neededExprs;
    /** @type {string} */
    let resultExpr;
    const currFnId = getActionId(this.currentRule, this.currentChoice);
    if (currFnId in this.actionReturnTypes) {
      neededExprs = exprVars.filter((_, i) => matchExprs[i].labeledExpr.label);
      resultExpr = Template.fnResultExpr({
        fnId: getActionId(this.currentRule, this.currentChoice),
        exprs: neededExprs.length > 0 ? neededExprs : [],
      });
    } else {
      neededExprs = exprVars.filter((_, i) => matchExprs[i].pluck);
      resultExpr = Template.strResultExpr({
        exprs: neededExprs.length > 0 ? neededExprs : exprVars,
      });
    }
    this.currentExpr = 0;

    if (node.action) this.actions.push(node.action.accept(this));
    return Template.union({
      exprs: node.exprs.map((expr) => {
        const translation = expr.accept(this);
        if (expr instanceof CST.Pluck) this.currentExpr++;
        return translation;
      }),
      startingRule: this.translatingStart,
      resultExpr,
    });
  }

  /**
   * @param {CST.Pluck} node
   * @this {Visitor}
   */
  visitPluck(node) {
    return node.labeledExpr.accept(this);
  }

  /**
   * @param {CST.Label} node
   * @this {Visitor}
   */
  visitLabel(node) {
    return node.annotatedExpr.accept(this);
  }

  /**
   * @param {CST.Annotated} node
   * @this {Visitor}
   */
  visitAnnotated(node) {
    if (node.qty && typeof node.qty === "string") {
      if (node.expr instanceof CST.Identificador) {
        // TODO: Implement quantifiers (i.e., ?, *, +)
        if (node.qty == "*") {
          let tipo = getReturnType(
            getActionId(node.expr.id, 0),
            this.actionReturnTypes
          );
          let code = '';
          if (tipo == 'character(len=:), allocatable') {
            code += `${getExprId(this.currentChoice, this.currentExpr)} = ""
                        temp = "-"
                        do while (.not. temp == "")
                            temp = ${node.expr
                .accept(this)
                .replace(/\(\)$/, "")}_kleene()
                            ${getExprId(
                  this.currentChoice,
                  this.currentExpr
                )} = ${getExprId(
                  this.currentChoice,
                  this.currentExpr
                )} // temp
                        end do
                        `;
          } else if (tipo == 'integer') {
            code += `${getExprId(this.currentChoice, this.currentExpr)} = -99999
                        tempi = -9999
                        do while (.not. tempi == -99999)
                        temp = intToStr(${getExprId(
              this.currentChoice,
              this.currentExpr
            )}) // intToStr(tempi)
                        ${getExprId(
              this.currentChoice,
              this.currentExpr
            )} = strToInt(temp)
                        tempi = ${node.expr
                .accept(this)
                .replace(/\(\)$/, "")}_kleene()
                        end do
                        `;
          }
          return code;
        }
        if (node.qty == "+") {
          let code = '';
          let tipo = getReturnType(
            getActionId(node.expr.id, 0),
            this.actionReturnTypes
          );
          if (tipo == 'character(len=:), allocatable') {
            code += `${getExprId(
              this.currentChoice,
              this.currentExpr
            )} = ${node.expr.accept(this)}
                        temp = "-"
                        do while (.not. temp == "")
                            temp = ${node.expr
                .accept(this)
                .replace(/\(\)$/, "")}_kleene()
                            ${getExprId(
                  this.currentChoice,
                  this.currentExpr
                )} = ${getExprId(
                  this.currentChoice,
                  this.currentExpr
                )} // temp
                        end do
                        `;
          }else if (tipo == 'integer') {
            code += `${getExprId(this.currentChoice, this.currentExpr)} = ${node.expr.accept(this)}
                        tempi = -9999
                        do while (.not. tempi == -99999)
                        temp = intToStr(${getExprId(
              this.currentChoice,
              this.currentExpr
            )}) // intToStr(tempi)
                        ${getExprId(
              this.currentChoice,
              this.currentExpr
            )} = strToInt(temp)
                        tempi = ${node.expr
                .accept(this)
                .replace(/\(\)$/, "")}_kleene()
                        end do
                        `;
          }
          return code;
        }
        if (node.qty == "?") {
          return `${getExprId(
            this.currentChoice,
            this.currentExpr
          )} = ${node.expr.accept(this).replace(/\(\)$/, "")}_kleene()
          `;
        }

        return `${getExprId(
          this.currentChoice,
          this.currentExpr
        )} = ${node.expr.accept(this)}`;
      }
      return Template.strExpr({
        quantifier: node.qty,
        expr: node.expr.accept(this),
        destination: getExprId(this.currentChoice, this.currentExpr),
      });
    } else if (node.qty) {
      if (node.expr instanceof CST.Identificador) {
        if (node.qty.length == 5){
          if (node.qty[2][0] != 0){
              return `
                  max_reps = ${node.qty[2][0]}
                  count = 0
                  ${getExprId(
                  this.currentChoice,
                  this.currentExpr)} = ${node.expr.accept(this)}
                  temp = "-"
                  count = count + 1
                  do while (count < max_reps .and. (.not. temp == ""))
                      
                          temp = ${node.expr.accept(this).replace(/\(\)$/, "")}_kleene()
                          ${getExprId(this.currentChoice, this.currentExpr)} = ${getExprId(this.currentChoice, this.currentExpr)} // temp
                          
                          if (.not. temp == "") then
                              count = count + 1
                          end if
                      
                  end do
                  !detectar minimo o maximo 
                  if ( count .NE. max_reps) then
                      cycle
                  end if 
                  `;
          }else{
              throw new Error('Cantida debe ser mayor a 0');
          }
      }else if(node.qty.length == 9){
          if (node.qty[4] == ".."){
              if (node.qty[2] == null &  node.qty[6] == null){
                  return `${getExprId(
                      this.currentChoice,
                      this.currentExpr)} = ""
                      temp = "-"
                      do while (.not. temp == "")
                          temp = ${node.expr.accept(this).replace(/\(\)$/, "")}_kleene()
                          ${getExprId(this.currentChoice, this.currentExpr)} = ${getExprId(this.currentChoice, this.currentExpr)} // temp
                      end do
                      `
              }else if (node.qty[2] == null){
                  return`
                  max_reps = ${node.qty[6][0]}  ! Número maximo de repeticiones permitidas
                  count = 0
                  ${getExprId(
                    this.currentChoice,
                    this.currentExpr)} = ""
                  temp = "-"
                  do while (count < max_reps .and. (.not. temp == ""))
                      temp = ${node.expr.accept(this).replace(/\(\)$/, "")}_kleene()
                          ${getExprId(this.currentChoice, this.currentExpr)} = ${getExprId(this.currentChoice, this.currentExpr)} // temp
                              
                          if (.not. temp == "") then
                              count = count + 1
                          end if
                          
                      end do
                      !detectar minimo o maximo 
                      if ( count > max_reps) then
                          cycle
                      end if 
                      `; 
              }else if (node.qty[6] == null){
                  return`
                          
                      min_reps = ${node.qty[2][0]}  ! Número mínimo de repeticiones permitidas
                      count = 0
                      ${getExprId(
                      this.currentChoice,
                      this.currentExpr)} = ${node.expr.accept(this)}
                      temp = "-"
                      count = count + 1
                      do while ( .not. temp == "")
                          
                          temp = ${node.expr.accept(this).replace(/\(\)$/, "")}_kleene()
                          ${getExprId(this.currentChoice, this.currentExpr)} = ${getExprId(this.currentChoice, this.currentExpr)} // temp
                          
                          if (.not. temp == "") then
                              count = count + 1
                          end if
                          
                      end do
                      !detectar minimo o maximo 
                      if (count < min_reps ) then
                          cycle
                      end if 
                          `;
              }else{
                  return `
                      
                      min_reps = ${node.qty[2][0]}  ! Número mínimo de repeticiones permitidas
                      max_reps = ${node.qty[6][0]}  ! Número máximo de repeticiones permitidas
                      ${getExprId(
                        this.currentChoice,
                        this.currentExpr)} = ""
                      temp = "-"
                      do while (count < max_reps .and. (.not. temp == ""))
                          temp = ${node.expr.accept(this).replace(/\(\)$/, "")}_kleene()
                          ${getExprId(this.currentChoice, this.currentExpr)} = ${getExprId(this.currentChoice, this.currentExpr)} // temp
                          
                          if (.not. temp == "") then
                              count = count + 1
                          end if
                      end do
                      !detectar minimo o maximo 
                      if (count < min_reps .or. count > max_reps) then
                          cycle
                      end if 
          `
              }
          }
      }
      } else {
        return Template.strExpr({
          quantifier: node.qty,
          expr: node.expr.accept(this),
          destination: getExprId(this.currentChoice, this.currentExpr),
        });
      }
    } else {
      if (node.expr instanceof CST.Identificador) {
        return `${getExprId(
          this.currentChoice,
          this.currentExpr
        )} = ${node.expr.accept(this)}`;
      }
      return Template.strExpr({
        expr: node.expr.accept(this),
        destination: getExprId(this.currentChoice, this.currentExpr),
      });
    }
  }

  /**
   * @param {CST.Assertion} node
   * @this {Visitor}
   */
  visitAssertion(node) {
    if (node.assertion instanceof CST.Identificador) {
      return `temp= ${node.assertion.accept(this)}`;
    }
    return Template.strExpr_Assertion({
      expr: node.assertion.accept(this),
      destination: getExprId(this.currentChoice, this.currentExpr),
    });
  }

  /**
   * @param {CST.NegAssertion} node
   * @this {Visitor}
   */
  visitNegAssertion(node) {
    if (node.assertion instanceof CST.Identificador) {
      return "\t\t\t\ttemp=" + getRuleId(node.assertion.id) + "_negative()";
    }

    return Template.strExpr_NegAssertion({
      expr: node.assertion.accept(this),
      destination: getExprId(this.currentChoice, this.currentExpr),
    });
  }

  /**
   * @param {CST.Predicate} node
   * @this {Visitor}
   */
  visitPredicate(node) {
    return Template.action({
      ruleId: this.currentRule,
      choice: this.currentChoice,
      signature: Object.keys(node.params),
      returnType: node.returnType,
      paramDeclarations: Object.entries(node.params).map(
        ([label, ruleId]) =>
          `${getReturnType(
            getActionId(ruleId, this.currentChoice),
            this.actionReturnTypes
          )} :: ${label}`
      ),
      code: node.code,
    });
  }

  /**
   * @param {CST.String} node
   * @this {Visitor}
   */
  visitString(node) {
    if (node.isCase == null || node.isCase == false) {
      return `acceptString('${node.val}')`;
    } else {
      return `acceptStringCI('${node.val}')`;
    }
  }

  /**
   * @param {CST.Clase} node
   * @this {Visitor}
   */
  visitClase(node) {
    // [abc0-9A-Z]
    let characterClass = [];
    const literalMap = {
      "\\t": "char(9)", // Tabulación
      "\\n": "char(10)", // Nueva línea
      " ": "char(32)", // Espacio
      "\\r": "char(13)", // Retorno de carro
    };
    const set = node.chars
      .filter((char) => typeof char === "string")
      .map((char) => {
        if (literalMap[char]) {
          return literalMap[char];
        } else if (node.isCase == null) {
          return `'${char}'`;
        } else {
          return `'${char.toLowerCase()}'`;
        }
      });
    const ranges = node.chars
      .filter((char) => char instanceof CST.Rango)
      .map((range) => {
        range.isCase = node.isCase;
        return range.accept(this);
      });
    if (set.length !== 0) {
      if (node.isCase == null || node.isCase == false) {
        characterClass = [`acceptSet([${set.join(",")}])`];
      } else {
        characterClass = [`acceptSetCI([${set.join(",")}])`];
      }
    }
    if (ranges.length !== 0) {
      characterClass = [...characterClass, ...ranges];
    }
    return `(${characterClass.join(" .or. ")})`; // acceptSet(['a','b','c']) .or. acceptRange('0','9') .or. acceptRange('A','Z')
  }

  /**
   * @param {CST.Rango} node
   * @this {Visitor}
   */
  visitRango(node) {
    if (node.isCase == null || node.isCase == false) {
      return `acceptRange('${node.bottom}', '${node.top}')`;
    } else {
      return `acceptRangeCI('${node.bottom}', '${node.top}')`;
    }
  }

  /**
   * @param {CST.Identificador} node
   * @this {Visitor}
   */
  visitIdentificador(node) {
    return getRuleId(node.id) + "()";
  }

  /**
   * @param {CST.Punto} node
   * @this {Visitor}
   */
  visitPunto(node) {
    return "acceptPeriod()";
  }

  /**
   * @param {CST.Fin} node
   * @this {Visitor}
   */
  visitFin(node) {
    return "if (.not. acceptEOF()) cycle";
  }
}
