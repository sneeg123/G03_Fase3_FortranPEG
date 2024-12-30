import Visitor from './Visitor.js';
import * as n from './CST.js';

export default class Tokenizer extends Visitor {
    constructor() {
        super();
        this.calledRules = [];
        this.pendingRules = [];
        this.isFisrtRule = true;
        this.nameProduction = '';
    }
    visitProducciones(node) {
        return `
        recursive function peg_${node.id}() result(accept)
            logical :: accept
            integer :: i
            integer :: count, min_reps, max_reps
            accept = .false.
            ${node.expr.accept(this)}
            ${
                node.start
                    ? `
                    if (.not. acceptEOF()) then
                        return
                    end if
                    `
                    : ''
            }
            accept = .true.
        end function peg_${node.id}
        `;
    }
    visitOpciones(node) {
        const template = `
        do i = 0, ${node.exprs.length}
            select case(i)
                ${node.exprs
                    .map(
                        (expr, i) => `
                        case(${i})
                            ${expr.accept(this)}
                            exit
                        `
                    )
                    .join('\n')}
            case default
                return
            end select
        end do
        `;
        return template;
    }
    
    visitUnion(node) {
        return node.exprs.map((expr) => expr.accept(this)).join('\n');
    }

    visitExpresion(node) {
        const condition = node.expr.accept(this);
        console.log(node.qty);
        
        const rangePattern = /\|(\d+)\.\.(\d+)\|/;  // |1..2|
        const singleNumPattern = /\|(\d+)\|/;       // |numero|
        const numWithDotsPattern = /\|(\d+)\.\.\|/; // |numero..|
        const dotsWithNumPattern = /\|\.\.(\d+)\|/; // |..numero|

        const match1 = rangePattern.exec(node.qty);
        const match2 = singleNumPattern.exec(node.qty);
        const match3 = numWithDotsPattern.exec(node.qty);
        const match4 = dotsWithNumPattern.exec(node.qty);

        switch (node.qty) {
            case '+':
                return `
                if (.not. (${condition})) then
                    cycle
                end if
                do while (.not. cursor > len(input))
                    if (.not. (${condition})) then
                        exit
                    end if
                end do
                `;
            case '*':
                return `
                do while (.not. cursor > len(input))
                    if (.not. (${condition})) then
                        exit
                    end if
                end do
                `;
            case '?':
                return `
                if (${condition}) then
                    
                end if
                `;
            case '|1..|':
                return `
                if (.not. (${condition})) then
                    cycle
                end if
                do while (.not. cursor > len(input))
                    if (.not. (${condition})) then
                        exit
                    end if
                end do
                `;
            case '|0..|':
                return `
                do while (.not. cursor > len(input))
                    if (.not. (${condition})) then
                        exit
                    end if
                end do
                `;
            case '|1|':
                return `
                if (.not. (${condition})) then
                    cycle
                end if
                `;
            case '|0|':
                return '';
            case '|0..1|':
                return `
                if (${condition}) then
                    
                end if
                `;
            case '|..|':
                return `
                do while (.not. cursor > len(input))
                    if (.not. (${condition})) then
                        exit
                    end if
                end do
                `;
            default:
                if (match1) {
                    const start = match1[1];
                    const end = match1[2];
                    console.log(start,end)
                    return `
                                min_reps = ${start}  ! Número mínimo de repeticiones permitidas
                                max_reps = ${end}  ! Número máximo de repeticiones permitidas
                                count = 0 
                                do while (count < max_reps)
                                    if (.not. (${condition})) then
                                        exit
                                    end if
                                    count = count + 1
                                end do
                                !detectar minimo o maximo 
                                if (count < min_reps .or. count > max_reps) then
                                    cycle
                                end if               
                    `;
                }else if (match2 !== null) {
                    const num = match2[1];
                    return `
                                max_reps = ${num}  ! Número de repeticiones permitidas
                                count = 0
                                do while (count < max_reps)
                                    if (.not. (${condition})) then
                                        exit
                                    end if
                                    count = count + 1
                                end do
                                !detectar minimo o maximo 
                                if ( count .NE. max_reps) then
                                    cycle
                                end if               
                    `;
                }else if (match3 !== null) {
                     // Aquí se entra si el patrón es |numero...|
                    const num = match3[1];
                    
                    return`
                                min_reps = ${num}  ! Número mínimo de repeticiones permitidas
                                count = 0
                                do while (.not. cursor > len(input))
                                    if (.not. (${condition})) then
                                        exit
                                    end if
                                    count = count + 1
                                end do  
                                !detectar minimo o maximo 
                                if (count < min_reps ) then
                                    cycle
                                end if               
                    `; 
                }else if (match4 !== null) {
                     // Aquí se entra si el patrón es |..numero|
                    const num = match4[1];
                    return`
                                max_reps = ${num}  ! Número maximo de repeticiones permitidas
                                count = 0
                                do while (count < max_reps)
                                    if (.not. (${condition})) then
                                        exit
                                    end if
                                    count = count + 1
                                end do  
                                !detectar minimo o maximo 
                                if (count > max_reps ) then
                                    cycle
                                end if               
                    `; 
                }
                return `
                if (.not. (${condition})) then
                    cycle
                end if
                `;
        }
    }

    visitString(node) {
        if (node.isCase == null){
            return `acceptString('${node.val}')`;
        }
        else {
            return `acceptStringCI('${node.val}')`;
        }
    }

    visitAny(node) { 
        return 'acceptPeriod()';
    }

    //falta
    visitCorchetes(node) {  
        let characterClass = [];
        const literalMap = {
            "\\t": "char(9)",  // Tabulación
            "\\n": "char(10)", // Nueva línea
            " ": "char(32)",   // Espacio
            "\\r": "char(13)",  // Retorno de carro
        };
        const set = node.exprs
            .filter((char) => typeof char === 'string')
            .map((char) => {
                if (literalMap[char]) {
                    return literalMap[char];
                } else if  (node.isCase == null) {
                    return `'${char}'`;
                } else {
                    return `'${char.toLowerCase()}'`;
                }
            });
        const ranges = node.exprs
            .filter((char) => char instanceof n.rango)
            
            .map((range) => {
                range.isCase = node.isCase;
                return range.accept(this);
            });
        if (set.length !== 0) {
            if (node.isCase == null){
            characterClass = [`acceptSet([${set.join(',')}])`];
            }else {
                characterClass = [`acceptSetCI([${set.join(',')}])`];
            }
        }
        if (ranges.length !== 0) {
            characterClass = [...characterClass, ...ranges];
        }
        return characterClass.join(' .or. '); 
    }

    visitrango(node) {
        
        if (node.isCase == null){
        return `acceptRange('${node.start}', '${node.end}')`;
        }else{
        return `acceptRangeCI('${node.start}', '${node.end}')`;
        }
    }

    //falta
    visitliteralRango(node) {
        const literalMap = {
            "\\t": "char(9)",  // Tabulación
            "\\n": "char(10)", // Nueva línea
            " ": "char(32)",   // Espacio
            "\\r": "char(13)",  // Retorno de carro
        };
    
        // Verifica si el literal es especial y tradúcelo, de lo contrario usa comillas
        const literalFortran = literalMap[node.val] || `"${node.val}"`;
    
        const condition = node.isCase
        ? `tolower(input(cursor:cursor)) == tolower(${literalFortran})`
        : `input(cursor:cursor) == ${literalFortran}`
        return "(" + condition + ")";
    }

    visitidRel(node) {
        console.log(node.val);
        return `peg_${node.val}()`;
    }
    //falta
    visitgrupo(node) {
        return node.expr.accept(this);
    }

    visitfinCadena(node) {
        return 'acceptEOF()';
    }

}
