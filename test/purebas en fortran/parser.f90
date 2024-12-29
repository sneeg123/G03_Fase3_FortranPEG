module parser
    implicit none
    integer, private :: cursor
    character(len=:), allocatable, private :: input, expected

    contains

    subroutine parse(str)
        character(len=:), allocatable, intent(in) :: str

        input = str
        cursor = 1
        expected = ''
        if (sum()) then
            print *, "Parsed input succesfully!"
        else
            call error()
        end if
    end subroutine parse

    subroutine error()
        if (cursor > len(input)) then
            print *, "Error: Expected "//expected//", but found <EOF>"
            call exit(1)
        end if
        print *, "Error: Expected "//expected//", but found '"//input(cursor:cursor)//"'"
        call exit(1)
    end subroutine error

    function sum() result(accept)
        logical :: accept
        integer :: i
        !*************************************************************
        !*************************************************************
        !*************************************************************
        !*************************************************************
        integer :: count, min_reps, max_reps !declarar en cada produccion para evitar probleams
        !*************************************************************
        !*************************************************************
        !*************************************************************
        !*************************************************************

        accept = .false.
        do i = 1, 4
            select case(i)
            case(1)
            !*************************************************************
            !*************************************************************
            !*************************************************************
            !para un * solo colocar esto un do while dentro del if un exit
            do while (.not. cursor > len(input))
                if (.not. White_space()) then
                    exit
                end if
            end do
            !*************************************************************
            !*************************************************************
            !*************************************************************
            if (.not. num()) then
                cycle
            end if
            do while (.not. cursor > len(input))
                if (.not. num()) then
                    exit
                end if
            end do

            !*************************************************************
            !*************************************************************
            !*************************************************************
            !para un ? solo 

            if (acceptString('.')) then
                
            end if

            if (num()) then
                
            end if
            !*************************************************************
            !*************************************************************
            !*************************************************************
            !para un  ? solo colocar un if el dato y nada dento del if 

            do while (.not. cursor > len(input))
                if (.not. White_space()) then
                    exit
                end if
            end do

            if (.not. acceptString('+')) then
                cycle
            end if

            do while (.not. cursor > len(input))
                if (.not. White_space()) then
                    exit
                end if
            end do

            if (.not. num()) then
                cycle
            end if
            do while (.not. cursor > len(input))
                if (.not. num()) then
                    exit
                end if
            end do

            if (.not. acceptPeriod()) then
                cycle
            end if
            exit


            case(2)

            !*************************************************************
            !*************************************************************
            !*************************************************************
            !*************************************************************
            min_reps = 1  ! Número mínimo de repeticiones permitidas
            max_reps = 2  ! Número máximo de repeticiones permitidas
            count = 0

            do while (count < max_reps)
                if (.not. letra()) then
                    exit
                end if
                count = count + 1
            end do
            !detectar minimo o maximo 
            if (count < min_reps .or. count > max_reps) then
                ! Manejar el error según sea necesario
                cycle
            end if
            
            ! logica para ver cantidad 
            !*************************************************************
            !*************************************************************
            !*************************************************************
            !*************************************************************
            !*************************************************************

            
            if(.not. acceptString('+')) then
                cycle
            end if


            !*************************************************************
            !*************************************************************
            !*************************************************************
            !*************************************************************
            !logica contardor y delimitador
        
            max_reps = 3  ! Número máximo de repeticiones permitidas
            count = 0

            do while (count < max_reps)
                if (.not. letra()) then
                    cycle  ! Manejar el error si no hay una letra
                end if
                count = count + 1

                ! Verificar si se necesita una coma después de la letra
                if (count < max_reps) then
                    if (.not. acceptString(',')) then
                        cycle  ! Manejar el error si no hay una coma
                    end if
                end if
            end do

            ! Verificar que el número de letras sea exactamente igual a max_reps
            if (count /= max_reps) then
                ! Manejar el error si no se cumple el patrón exacto
                cycle
            end if

            ! Verificar que no termine con una coma
            if (input(cursor-1:cursor-1) == ',') then
                cycle  ! Manejar el error si termina con una coma
            end if

            !*************************************************************
            !*************************************************************
            !*************************************************************
            !*************************************************************
            !*************************************************************


            if (.not. White_space()) then
                cycle
            end if

            !*************************************************************
            !*************************************************************
            !*************************************************************
            !*************************************************************
            !*************************************************************
            !*************************************************************
            min_reps = 2  ! Número mínimo de repeticiones permitidas
            max_reps = 8  ! Número máximo de repeticiones permitidas
            count = 0


            ! Verificar la primera letra
            if (.not. letra()) then
                cycle  ! Manejar el error si no hay una letra al inicio
            end if
            count = count + 1

            ! Entrar en el ciclo para verificar comas y letras
            do while (count < max_reps)
                if (.not. acceptString(',')) then
                    exit  ! Salir si no hay una coma
                end if

                if (.not. letra()) then
                    cycle  ! Manejar el error si no hay una letra después de la coma
                end if
                count = count + 1
            end do

            ! Verificar que el número de letras esté dentro del rango permitido
            if (count < min_reps .or. count > max_reps) then
                ! Manejar el error si no se cumple el patrón exacto
                cycle
            end if

            ! Verificar que no termine con una coma
            if (input(cursor-1:cursor-1) == ',') then
                cycle  ! Manejar el error si termina con una coma
            end if
            !*************************************************************
            !*************************************************************
            !*************************************************************
            !*************************************************************
            !*************************************************************
            !*************************************************************
            exit

            case(3)

                if(.not. acceptString('/')) then
                    cycle
                end if

                if(acceptString('-')) then
                    cycle
                end if
                

                if (.not. num()) then
                    cycle
                end if
            exit
            case default
                return
            end select
        end do
        if (.not. acceptEOF()) then
            return
        end if
        accept = .true.
    end function sum
    !*************************************************************
    !*************************************************************
    !*************************************************************
    !*************************************************************
    !*************************************************************
    !const literalMap = {
    !        "\\t": "char(9)",  // Tabulación
    !        "\\n": "char(10)", // Nueva línea
    !        " ": "char(32)",   // Espacio
    !        "\\r": "char(13)",  // Retorno de carro
    !   };
    !usar esto para javscript
    function White_space() result(accept)
        logical :: accept
        !por cada conjunto characteress crear esto 
        character(len=1), dimension(4) :: set

        set = [char(9), char(10), char(32), char(13)]
        !por cada conjunto characteress crear esto
        accept = .false.
        if (.not. acceptSet(set)) then
            expected = '[ \n\t\r]'
            return
        end if
        accept = .true.
    end function White_space
    !*************************************************************
    !*************************************************************
    !*************************************************************
    !*************************************************************
    !*************************************************************
    !*************************************************************

    function num() result(accept)
        logical :: accept

        accept = .false.
        if(.not. acceptRange('0', '9')) then
            expected = '[0-9]'
            return
        end if
        accept = .true.
    end function num

    function letra() result(accept)
        logical :: accept

        accept = .false.
        if(.not. acceptRange('a', 'z')) then
            expected = '[a-z]'
            return
        end if
        accept = .true.
    end function letra

    function acceptString(str) result(accept)
        character(len=*) :: str
        logical :: accept
        integer :: offset

        offset = len(str) - 1
        if (str /= input(cursor:cursor + offset)) then
            accept = .false.
            expected = str
            return
        end if
        cursor = cursor + len(str);
        accept = .true.
    end function acceptString

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

    function acceptPeriod() result(accept)
        logical :: accept

        if (cursor > len(input)) then
            accept = .false.
            expected = "<ANYTHING>"
            return
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptPeriod

    function acceptEOF() result(accept)
        logical :: accept

        if(.not. cursor > len(input)) then
            accept = .false.
            expected = "<EOF>"
            return
        end if
        accept = .true.
    end function acceptEOF



    
end module parser

