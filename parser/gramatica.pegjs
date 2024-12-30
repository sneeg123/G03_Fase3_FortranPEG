{{
    import { ids, usos} from '../index.js'
    import { ErrorReglas } from './error.js';
    import { errores } from '../index.js'
    import * as n from '../parser/visitor/CST.js';
}}

gramatica
  = _ block:block? _ prods:producciones+ _ {

    console.log("Block al inicio", block);

    let duplicados = ids.filter((item, index) => ids.indexOf(item) !== index);
    if (duplicados.length > 0) {
        errores.push(new ErrorReglas("Regla duplicada: " + duplicados[0]));
    }

    // Validar que todos los usos están en ids
    let noEncontrados = usos.filter(item => !ids.includes(item));
    if (noEncontrados.length > 0) {
        errores.push(new ErrorReglas("Regla no encontrada: " + noEncontrados[0]));
    }
    prods[0].start = true;
    return prods;
  }

producciones
  = _ id:identificador _ alias:$(literales)? _ "=" _ expr:opciones _ llaves:block? (_";")? {
    ids.push(id);
    console.log("LLAVES", llaves);
    return new n.Producciones(id, expr, alias);
  }

opciones
  = expr:union rest:(_ "/" _ @union)* _ llaves:block_parentesis*{
    console.log("llaves en parentesis", llaves)
    return new n.Opciones([expr, ...rest]);
  }

union
  = expr:expresion rest:(_ @expresion !(_ literales? _ "=") )* {
    return new n.Union([expr, ...rest]);
  }

expresion // @"suma" &{}
  = label:$(etiqueta/varios/Pluck)? varios:varios? _ expr:expresiones _ qty:$([?+*]/conteo)? {
    console.log("LABEL", label)
    return new n.Expresion(expr, label, qty, varios);
  }

etiqueta = Pluck? _ id:identificador _ ":" (varios/ Pluck)?

Pluck = "@" 

varios = ("!"(!".") /"$"/"&")

expresiones
  = id:identificador {
    usos.push(id);
    return new n.idRel(id);
  }
  / val:$literales isCase:"i"? {
    return new n.String(val.replace(/['"]/g, ''), isCase);
  }
  / "(" _ @opciones _ ")"
  / exprs:corchetes isCase:"i"?{
    //console.log("Corchetes", exprs);
    return new n.Corchetes(exprs, isCase);

  }
  / "." {
    return new n.Any(true);
  }
  / "!."{
    return new n.finCadena();
  }

// conteo = "|" parteconteo _ (_ delimitador )? _ "|"

conteo = "|" _ (numero / id:identificador) _ "|"
        / "|" _ (numero / id:identificador)? _ ".." _ (numero / id2:identificador)? _ "|"
        / "|" _ (numero / id:identificador)? _ "," _ opciones _ "|"
        / "|" _ (numero / id:identificador)? _ ".." _ (numero / id2:identificador)? _ "," _ opciones _ "|"

// parteconteo = identificador
//             / [0-9]? _ ".." _ [0-9]?
// 			/ [0-9]

// delimitador =  "," _ expresion

// Regla principal que analiza corchetes con contenido
corchetes
    = "[" contenido:(rango)+ "]" {
        return contenido;
    }

block_parentesis = _ "(" _ content:block _ ")" _  {
    return content;
}

block
  = "{" content:blockContent"}" {
      return content
  } 

blockContent
  = (block / [^{}])* {
      return text();
  }

// Regla para validar un rango como [A-Z]
rango
    = inicio:$caracter "-" fin:$caracter {
        return new  n.rango(inicio, fin);
    } 
    / $texto

// Regla para caracteres individuales
caracter
    = [a-zA-Z0-9_ ] 

// Coincide con cualquier contenido que no incluya "]"
contenido
    = contenido: (@$texto){
        // return new n.literalRango(contenido);
    }

corchete
    = "[" contenido "]"

texto
    = "\\" escape
    /[^\[\]]

literales
  = '"' @stringDobleComilla* '"'
  / "'" @stringSimpleComilla* "'"

stringDobleComilla = !('"' / "\\" / finLinea) .
                    / "\\" escape
                    / continuacionLinea

stringSimpleComilla = !("'" / "\\" / finLinea) .
                    / "\\" escape
                    / continuacionLinea

continuacionLinea = "\\" secuenciaFinLinea

finLinea = [\n\r\u2028\u2029]

escape = "'"
        / '"'
        / "\\"
        / "b"
        / "f"
        / "n"
        / "r"
        / "t"
        / "v"
        / "u"

secuenciaFinLinea = "\r\n" / "\n" / "\r" / "\u2028" / "\u2029"

// literales = 
//     "\"" [^"]* "\""
//     / "'" [^']* "'"
    

numero = [0-9]+

identificador = [_a-z]i[_a-z0-9]i* { return text() }


_ = (Comentarios /[ \t\n\r])*


Comentarios = 
    "//" [^\n]* 
    / "/*" (!"*/" .)* "*/"
