/**
 *
 * @param {{
*  beforeContains: string
*  afterContains: string
*  startingRuleId: string;
*  startingRuleType: string;
*  rules: string[];
*  actions: string[];
* }} data
* @returns {string}
*/
export const main = (data) => `
!auto-generated
module parser
  implicit none
  character(len=:), allocatable, private :: input
  integer, private :: savePoint, lexemeStart, cursor

  interface toStr
      module procedure intToStr
      module procedure strToStr
  end interface
  
  ${data.beforeContains}

  contains
  
  ${data.afterContains}

  function parse(str) result(res)
      character(len=:), allocatable :: str
      ${data.startingRuleType} :: res

      input = str
      cursor = 1

      res = ${data.startingRuleId}()
  end function parse

  ${data.rules.join("\n")}

  ${data.actions.join("\n")}

  function acceptString(str) result(accept)
      character(len=*) :: str
      logical :: accept
      integer :: offset

      offset = len(str) - 1
      if (str /= input(cursor:cursor + offset)) then
          accept = .false.
          return
      end if
      cursor = cursor + len(str)
      accept = .true.
  end function acceptString

  function acceptStringCI(str) result(accept)
       character(len=*) :: str
       logical :: accept
       integer :: offset
       character(len=len(str)) :: lower_input
   
       offset = len(str) - 1
       lower_input = tolower(input(cursor:cursor + offset))
       if (tolower(str) /= lower_input) then
           accept = .false.
           return
       end if
       cursor = cursor + len(str)
       accept = .true.
   end function acceptStringCI


  function acceptRange(bottom, top) result(accept)
      character(len=1) :: bottom, top
      logical :: accept

      if(.not. (input(cursor:cursor) >= bottom .and. input(cursor:cursor) <= top)) then
          accept = .false.
          return
      end if
      cursor = cursor + 1
      accept = .true.
  end function acceptRange

  function acceptRangeCI(bottom, top) result(accept)
       character(len=1) :: bottom, top
       logical :: accept
       character(len=1) :: lower_input

       lower_input = tolower(input(cursor:cursor))
       if (.not. (lower_input >= tolower(bottom) .and. lower_input <= tolower(top))) then
           accept = .false.
           return
       end if
       cursor = cursor + 1
       accept = .true.
   end function acceptRangeCI

  function acceptSet(set) result(accept)
      character(len=1), dimension(:) :: set
      logical :: accept

      if(.not. (findloc(set, input(cursor:cursor), 1) > 0)) then
          accept = .false.
          return
      end if
      cursor = cursor + 1
      accept = .true.
  end function acceptSet

   

   function acceptSetCI(set) result(accept)
       character(len=1), dimension(:) :: set
       logical :: accept
       character(len=1) :: lower_input

       lower_input = tolower(input(cursor:cursor))
       if (.not. (findloc(set, lower_input, 1) > 0)) then
           accept = .false.
           return
       end if
       cursor = cursor + 1
       accept = .true.
   end function acceptSetCI

  function acceptPeriod() result(accept)
      logical :: accept

      if (cursor > len(input)) then
          accept = .false.
          return
      end if
      cursor = cursor + 1
      accept = .true.
  end function acceptPeriod

  function acceptEOF() result(accept)
      logical :: accept

      if(.not. cursor > len(input)) then
          accept = .false.
          return
      end if
      accept = .true.
  end function acceptEOF

  function consumeInput() result(substr)
      character(len=:), allocatable :: substr

      substr = input(lexemeStart:cursor - 1)
  end function consumeInput

  subroutine pegError()
      print '(A,I1,A)', "Error at ", cursor, ": '"//input(cursor:cursor)//"'"

      call exit(1)
  end subroutine pegError

  function intToStr(int) result(cast)
      integer :: int
      character(len=31) :: tmp
      character(len=:), allocatable :: cast
      if (int == -99999) then
             cast = ""
             return
        end if
       if (int == -9999) then
               cast = ""
               return
            end if
      write(tmp, '(I0)') int
      cast = trim(adjustl(tmp))
  end function intToStr

  function strToStr(str) result(cast)
      character(len=:), allocatable :: str
      character(len=:), allocatable :: cast

      cast = str
  end function strToStr

  function strToInt(str) result(cast)
        character(len=:), allocatable :: str
           integer :: cast
           if (len(trim(str)) == 0) then
               cast = -9999
               return
           end if
           read(str, *) cast
   end function strToInt

   function acceptStringDelim(str) result(accept)
     character(len=*) :: str
     logical :: accept
     integer :: offset
     character(len=:), allocatable :: beforeDelim, afterDelim, prueba

     offset = len(str) - 1

         ! Verificar si el delimitador coincide con la parte actual de la cadena
             if (str /= input(cursor:cursor + offset)) then
                 accept = .false.
                 return
             end if

             ! Realizar el corte
                 beforeDelim = input(1:cursor-1)       ! Parte antes del lexemeStart

             afterDelim = input(cursor+len(str):)           ! Parte después del delimitador

             input =beforeDelim // afterDelim
             
             ! Mover el cursor después del delimitador
             accept = .true.
       end function acceptStringDelim



  function tolower(str) result(lower_str)
       character(len=*), intent(in) :: str
       character(len=len(str)) :: lower_str
       integer :: i

       lower_str = str 
       do i = 1, len(str)
           if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
               lower_str(i:i) = achar(iachar(str(i:i)) + 32)
           end if
       end do
   end function tolower

end module parser
`;

/**
*
* @param {{
*  id: string;
*  returnType: string;
*  exprDeclarations: string[];
*  expr: string;
* }} data
* @returns
*/
export const rule = (data) => `
  function peg_${data.id}() result (res)
      ${data.returnType} :: res
      ${data.exprDeclarations.join("\n")}
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote

      savePoint = cursor
      ${data.expr}
  end function peg_${data.id}

   function peg_${data.id}_negative() result (res)
      ${data.returnType} :: res
      ${data.exprDeclarations.join("\n")}
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps
      integer :: i, tempi
      logical :: pivote

      savePoint = cursor
       ${data.expr.replace(/if\(\.not\./g, "if(")}
   end function peg_${data.id}_negative

  function peg_${data.id}_kleene() result (res)
      ${data.returnType} :: res
      ${data.exprDeclarations.join("\n")}
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote

      savePoint = cursor
      ${getReplaceKleene(data)}
  end function peg_${data.id}_kleene
`;

function getReplaceKleene(data) {
 if (data.returnType == "character(len=:), allocatable") {
   return `${data.expr.replace(
     /case default[\s\S]*?call pegError\(\)/,
     'case default\n        res = ""'
   )}`;
 } else if (data.returnType == "integer") {
   return `${data.expr.replace(
     /case default[\s\S]*?call pegError\(\)/,
     "case default\n        res = -99999"
   )}`;
 } else {
   return ` `;
 }
}

/**
*
* @param {{
*  exprs: string[]
* }} data
* @returns
*/
export const election = (data) => `
      do i = 0, ${data.exprs.length}
          select case(i)
          ${data.exprs
            .map(
              (expr, i) => `
          case(${i})
              cursor = savePoint
              ${expr}
              exit
          `
            )
            .join("")}
          case default
              call pegError()
          end select
      end do
`;

/**
*
* @param {{
*  exprs: string[]
*  startingRule: boolean
*  resultExpr: string
*  assertion: boolean
*  negativeAssertion: boolean
* }} data
* @returns
*/
export const union = (data) => `
              ${data.exprs.join("\n")}
              ${data.startingRule ? "if (.not. acceptEOF()) cycle" : ""}
              ${data.resultExpr}
              ${
                data.assertion
                  ? "if (.not. res)then \n \t\t\t\tcycle \n \t\t\t\tend if"
                  : ""
              }
               ${
                 data.negativeAssertion
                   ? "if (res)then \n \t\t\t\tcycle \n \t\t\t\tend if"
                   : ""
               }

`;

/**
*
* @param {{
*  expr: string;
*  destination: string
*  quantifier?: string;
* }} data
* @returns
*/
export const strExpr = (data) => {
 if (!data.quantifier) {
   return `
               lexemeStart = cursor
               if(.not. ${data.expr}) cycle
               ${data.destination} = consumeInput()
       `;
 }
 switch (data.quantifier) {
   case "+":
     return `
               lexemeStart = cursor
               if (.not. ${data.expr}) cycle
               do while (.not. cursor > len(input))
                   if (.not. ${data.expr}) exit
               end do
               ${data.destination} = consumeInput()
           `;
   case "*":
     return `
                lexemeStart = cursor
                do while (.not. cursor > len(input))
                     if (.not. ${data.expr}) exit
                end do
                ${data.destination} = consumeInput()
              `;
   case "?":
     return `
                    lexemeStart = cursor
                    if (.not. ${data.expr}) cycle
                    ${data.destination} = consumeInput()
                `;
   default:
     console.log(data.quantifier);
     if(data.conteos){
        if(data.delim != null || data.delim !=  undefined){
            if(data.conteos[1] === "simple"){
                return `
               max_reps = ${data.conteos[0]}()
               count = 0
               lexemeStart = cursor
               do while (count < max_reps)
                   if (.not. ${data.expr}) then
                       exit
                   end if
                   count = count + 1
                   if (count < max_reps) then
                     if (.not. acceptStringDelim('${data.delim}')) then
                       exit
                     end if
                   end if
                 end do
                 if ( count .NE. max_reps) then
                     cycle
                 end if 
                 ${data.destination} = consumeInput()
                               
                           `;
               }else if(data.conteos[1] === "inicial"){
                return `
                lexemeStart = cursor
                min_reps = ${data.conteos[0]}()  ! Número mínimo de repeticiones permitidas
                count = 0
                if ((${data.expr})) then
                    count = count + 1
                    do while ( .not. cursor > len(input))
                    if (.not. acceptStringDelim('${data.delim}')) then
                        pivote = .false.
                        exit
                    end if
                    if (.not. ${data.expr}) then 
                        pivote = .true.
                        exit
                    end if
                    count = count + 1
                end do
                if (pivote)then
                        cycle
                end if
                end if
                
                !detectar minimo o maximo 
                if (count < min_reps ) then
                    cycle
                end if
                    ${data.destination} = consumeInput()                   
                            `;
       
               }else if(data.conteos[1] === "final"){ 
                return `
                max_reps = ${data.conteos[0]}()
                count = 0
                pivote = .false.
                lexemeStart = cursor


                if (${data.expr}) then
                count = count + 1
                    do while ( count < max_reps)
                        if (.not. acceptStringDelim('${data.delim}')) then
                            pivote = .false.
                            exit
                        end if
                        if (.not. ${data.expr}) then 
                            pivote = .true.
                            exit
                        end if
                    count = count + 1
                    end do
                    
                    if (pivote)then
                        cycle
                    end if
                end if
                if (count > max_reps ) then
                    cycle
                end if 
                ${data.destination} = consumeInput()
                
            `;
               }else if(data.conteos.length > 2 && data.conteos[2] == "doble"){
                return `
                ! DOBLES CON DELIM
                lexemeStart = cursor
                min_reps = ${data.conteos[0]}()  ! Número mínimo de repeticiones permitidas
                max_reps = ${data.conteos[1]}()  ! Número máximo de repeticiones permitidas
                count = 0 
                if ((${data.expr})) then
                                count = count + 1
                                do while ( .not. cursor > len(input))
                                if (.not. acceptStringDelim('${data.delim}')) then
                                    pivote = .false.
                                    exit
                                end if
                                if (.not. ${data.expr}) then 
                                    pivote = .true.
                                    exit
                                end if
                                count = count + 1
                            end do
                            if (pivote)then
                                    cycle
                            end if
                            end if
                !detectar minimo o maximo 
                if (count < min_reps .or. count > max_reps) then
                    cycle
                end if 
                ${data.destination} = consumeInput()                      
        `;
               }
        }
        if(data.conteos[1] === "simple"){
         return `
                       max_reps = ${data.conteos[0]}()
                       lexemeStart = cursor
                       count = 0
                       do while (count < max_reps)
                           if (.not.  ${data.expr}) then
                               exit
                           end if
                           count = count + 1
                       end do
                       !detectar minimo o maximo 
                       if ( count .NE. max_reps) then
                           cycle
                       end if   
                       ${data.destination} = consumeInput()
                   `;
        }else if(data.conteos[1] === "inicial"){
            return `
                               lexemeStart = cursor
                               min_reps = ${data.conteos[0]}()  ! Número mínimo de repeticiones permitidas
                               count = 0
                               do while (.not. cursor > len(input))
                                   if (.not. (${data.expr})) then
                                       exit
                                   end if
                                   count = count + 1
                               end do  
                               !detectar minimo o maximo 
                               if (count < min_reps ) then
                                   cycle
                               end if
                               ${data.destination} = consumeInput()                   
                           `;

        }else if(data.conteos[1] === "final"){ 
            return `
                       lexemeStart = cursor
                       max_reps = ${data.conteos[0]}()  ! Número maximo de repeticiones permitidas
                       count = 0
                       do while (count < max_reps)
                           if (.not. (${data.expr})) then
                               exit
                           end if
                           count = count + 1
                       end do  
                       !detectar minimo o maximo 
                       if (count > max_reps ) then
                           cycle
                       end if 
                       ${data.destination} = consumeInput()          
                       `;
        }else if(data.conteos.length > 2 && data.conteos[2] == "doble"){
            return `
                               lexemeStart = cursor
                               min_reps = ${data.conteos[0]}()  ! Número mínimo de repeticiones permitidas
                               max_reps = ${data.conteos[1]}()  ! Número máximo de repeticiones permitidas
                               count = 0 
                               do while (count < max_reps)
                                   if (.not. (${data.expr})) then
                                       exit
                                   end if
                                   count = count + 1
                               end do
                               !detectar minimo o maximo 
                               if (count < min_reps .or. count > max_reps) then
                                   cycle
                               end if 
                               ${data.destination} = consumeInput()                      
                   `;
        }
     }

     if (data.quantifier.length == 5) {
       if (data.quantifier[2][0] != 0) {
         return `
                       max_reps = ${data.quantifier[2][0]}
                       lexemeStart = cursor
                       count = 0
                       do while (count < max_reps)
                           if (.not.  ${data.expr}) then
                               exit
                           end if
                           count = count + 1
                       end do
                       !detectar minimo o maximo 
                       if ( count .NE. max_reps) then
                           cycle
                       end if   
                       ${data.destination} = consumeInput()
                   `;
       } else {
         throw new Error("Cantida debe ser mayor a 0");
       }
     } else if (data.quantifier.length == 9) {
       if (data.quantifier[4] == "..") {
         if ((data.quantifier[2] == null) & (data.quantifier[6] == null)) {
           return `
                           lexemeStart = cursor
                           do while (.not. cursor > len(input))
                               if (.not. ${data.expr}) exit
                           end do
                           ${data.destination} = consumeInput()
                       `;
         } else if (data.quantifier[2] == null) {
            //..3
           return `
                       lexemeStart = cursor
                       max_reps = ${data.quantifier[6][0]}  ! Número maximo de repeticiones permitidas
                       count = 0
                       do while (count < max_reps)
                           if (.not. (${data.expr})) then
                               exit
                           end if
                           count = count + 1
                       end do  
                       !detectar minimo o maximo 
                       if (count > max_reps ) then
                           cycle
                       end if 
                       ${data.destination} = consumeInput()          
                       `;
         } else if (data.quantifier[6] == null) {
            //1..
           return `
                               lexemeStart = cursor
                               min_reps = ${data.quantifier[2][0]}  ! Número mínimo de repeticiones permitidas
                               count = 0
                               do while (.not. cursor > len(input))
                                   if (.not. (${data.expr})) then
                                       exit
                                   end if
                                   count = count + 1
                               end do  
                               !detectar minimo o maximo 
                               if (count < min_reps ) then
                                   cycle
                               end if
                               ${data.destination} = consumeInput()                   
                           `;
         } else {
            //1..3
           return `
                               lexemeStart = cursor
                               min_reps = ${data.quantifier[2][0]}  ! Número mínimo de repeticiones permitidas
                               max_reps = ${data.quantifier[6][0]}  ! Número máximo de repeticiones permitidas
                               count = 0 
                               do while (count < max_reps)
                                   if (.not. (${data.expr})) then
                                       exit
                                   end if
                                   count = count + 1
                               end do
                               !detectar minimo o maximo 
                               if (count < min_reps .or. count > max_reps) then
                                   cycle
                               end if 
                               ${data.destination} = consumeInput()                      
                   `;
         }
       }
       if (data.quantifier[4] == ",") {
         if (data.quantifier[2][0] != 0) {
           const coma = data.quantifier[6].exprs[0].exprs[0].labeledExpr.annotatedExpr.expr.val;
           console.log(coma); 
             return `
               max_reps = ${data.quantifier[2][0]}
               count = 0
               lexemeStart = cursor
               do while (count < max_reps)
                   if (.not. ${data.expr}) then
                       exit
                   end if
                   count = count + 1
                   if (count < max_reps) then
                     if (.not. acceptStringDelim('${coma}')) then
                       exit
                     end if
                   end if
                 end do
                 if ( count .NE. max_reps) then
                     cycle
                 end if 
                 ${data.destination} = consumeInput()
                               
                           `;
             } else {
               throw new Error("Cantida debe ser mayor a 0");
             }
       }
     }else if (data.quantifier.length == 13){
       console.log(data.quantifier);
       if ((data.quantifier[2] == null) & (data.quantifier[6] == null)) {
           const coma = data.quantifier[10].exprs[0].exprs[0].labeledExpr.annotatedExpr.expr.val;
           console.log(coma);
           return `
                       pivote = .false.
                       lexemeStart = cursor
                       

                       if (${data.expr}) then
                           do while ( .not. cursor > len(input))
                               if (.not. acceptStringDelim('${coma}')) then
                                   pivote = .false.
                                   exit
                               end if
                               if (.not. ${data.expr}) then 
                                   pivote = .true.
                                   exit
                               end if
                               
                           end do
                           if (pivote)then
                               cycle
                           end if
                       end if
           
                       ${data.destination} = consumeInput()
                   `;
       }else if (data.quantifier[2] == null) {
               const coma = data.quantifier[10].exprs[0].exprs[0].labeledExpr.annotatedExpr.expr.val;
               console.log(coma);
               return `
                           max_reps = ${data.quantifier[6][0]}
                           count = 0
                           pivote = .false.
                           lexemeStart = cursor


                           if (${data.expr}) then
                           count = count + 1
                               do while ( count < max_reps)
                                   if (.not. acceptStringDelim('${coma}')) then
                                       pivote = .false.
                                       exit
                                   end if
                                   if (.not. ${data.expr}) then 
                                       pivote = .true.
                                       exit
                                   end if
                               count = count + 1
                               end do
                               
                               if (pivote)then
                                   cycle
                               end if
                           end if
                           if (count > max_reps ) then
                               cycle
                           end if 
                           ${data.destination} = consumeInput()
                           
                       `;
           }else if (data.quantifier[6] == null){
               const coma = data.quantifier[10].exprs[0].exprs[0].labeledExpr.annotatedExpr.expr.val;
               console.log(coma);
               return `
                           lexemeStart = cursor
                           min_reps = ${data.quantifier[2][0]}  ! Número mínimo de repeticiones permitidas
                           count = 0
                           if ((${data.expr})) then
                               count = count + 1
                               do while ( .not. cursor > len(input))
                               if (.not. acceptStringDelim('${coma}')) then
                                   pivote = .false.
                                   exit
                               end if
                               if (.not. ${data.expr}) then 
                                   pivote = .true.
                                   exit
                               end if
                               count = count + 1
                           end do
                           if (pivote)then
                                   cycle
                           end if
                           end if
                           
                           !detectar minimo o maximo 
                           if (count < min_reps ) then
                               cycle
                           end if
                               ${data.destination} = consumeInput()                   
                                       `;
           }else{
               const coma = data.quantifier[10].exprs[0].exprs[0].labeledExpr.annotatedExpr.expr.val;
               console.log(coma);
               return `
               lexemeStart = cursor
               min_reps = ${data.quantifier[2][0]}  ! Número mínimo de repeticiones permitidas
               max_reps = ${data.quantifier[6][0]}  ! Número máximo de repeticiones permitidas
               count = 0 
               if ((${data.expr})) then
                               count = count + 1
                               do while ( .not. cursor > len(input))
                               if (.not. acceptStringDelim('${coma}')) then
                                   pivote = .false.
                                   exit
                               end if
                               if (.not. ${data.expr}) then 
                                   pivote = .true.
                                   exit
                               end if
                               count = count + 1
                           end do
                           if (pivote)then
                                   cycle
                           end if
                           end if
               !detectar minimo o maximo 
               if (count < min_reps .or. count > max_reps) then
                   cycle
               end if 
               ${data.destination} = consumeInput()                      
       `;
           }

   }

     throw new Error(`'${data.quantifier}' quantifier needs implementation`);
 }
};

export const strExpr_Assertion = (data) => {
 return `
               if(.not. ${data.expr}) cycle
       `;
};

export const strExpr_NegAssertion = (data) => {
 return `
               if(${data.expr}) cycle
       `;
};

/**
*
* @param {{
*  exprs: string[];
* }} data
* @returns
*/
export const strResultExpr = (data) => {
 if (data.exprs && data.exprs.length > 0) {
   return `
              res = ${data.exprs.map((expr) => `toStr(${expr})`).join("//")}
   `;
 } else {
   return ` `;
 }
};

/**
*
* @param {{
*  fnId: string;
*  exprs: string[];
* }} data
* @returns
*/
export const fnResultExpr = (data) => `
              res = ${data.fnId}(${data.exprs.join(", ")})
`;

/**
*
* @param {{
*  ruleId: string;
*  choice: number
*  signature: string[];
*  returnType: string;
*  paramDeclarations: string[];
*  code: string;
* }} data
* @returns
*/
export const action = (data) => {
 const signature = data.signature.join(", ");
 return `
  function peg_${data.ruleId}_f${data.choice}(${signature}) result(res)
      ${data.paramDeclarations.join("\n")}
      ${data.returnType} :: res
      ${data.code}
  end function peg_${data.ruleId}_f${data.choice}
  `;
};

export const actionConteo = (data) => {
   const signature = data.signature ? data.signature.join(", ") : '';
   return `
    function peg_${data.ruleId}_conteo${data.choice}(${signature}) result(res)
        ${data.paramDeclarations ? data.paramDeclarations.join("\n") : ''}
        ${data.returnType} :: res
        ${data.code}
    end function peg_${data.ruleId}_conteo${data.choice}
    `;
 };