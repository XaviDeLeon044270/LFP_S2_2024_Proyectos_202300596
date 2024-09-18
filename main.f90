program main
    implicit none
    integer :: i, len, linea, columna, columna_inicio, estado, puntero, numErrores, numTokens, ios, espacio_texto
    character(len=10000) :: contenido, buffer
    character(len=10) :: str_codigo_ascii, str_columna, str_linea
    character(len=1) :: char
    character(len=100) :: lexema
    character(len=1), dimension(26) :: A
    character(len=1), dimension(5) :: S
    character(len=1) :: char_error

    type :: ErrorInfo
        character(len=10) :: caracter  ! caracter
        character(len=100) :: descripcion  ! Descripción del error
        integer :: columna      ! Columna donde ocurrió el error
        integer :: linea        ! Línea donde ocurrió el error
    end type ErrorInfo

    type :: Token
        character(len=100) :: lexema
        character(len=20) :: tipo
        integer :: columna
        integer :: linea
    end type Token
    
    type(ErrorInfo), dimension(250) :: errores
    type(Token), dimension(250) :: tokens

    A = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
    S = [':', ';', '{', '}', '"']

    estado = 0
    puntero = 1
    columna = 1
    linea = 1
    numErrores = 0
    numTokens = 0
    contenido = ''
    lexema = ''

    do
        read(*, '(A)', IOSTAT=ios) buffer
        if (ios /= 0) exit 
        contenido = trim(contenido) // trim(buffer) // new_line('a') ! concatenamos el 
        !contenido mas lo que viene en el buffer y como leemos por el salto de linea al final
    end do

    len = len_trim(contenido)

    do while(puntero <= len)
        char = contenido(puntero:puntero)
        print *, char

        if(ichar(char) == 10) then
            columna = 1
            linea = linea + 1
            puntero = puntero + 1

        elseif(ichar(char) == 9) then
            columna = columna + 4
            puntero = puntero + 1

        elseif(ichar(char) == 32) then
            columna = columna + 1
            puntero = puntero + 1

        else
            select case (estado)
            case(0)
                if (any(char == A)) then
                    if (trim(lexema) == '') then
                        columna_inicio = columna
                    end if
                    estado = 0
                    columna = columna + 1
                    lexema = trim(lexema) // char
                elseif (any(char == S)) then
                    if (trim(lexema) == 'grafica') then
                        numTokens = numTokens + 1
                        print *, "lexema: ", lexema
                        tokens(numTokens) = Token(lexema, "Palabra reservada", columna_inicio, linea)
                        estado = 1
                    elseif (trim(lexema) == 'nombre') then
                        numTokens = numTokens + 1
                        print *, "lexema: ", lexema
                        tokens(numTokens) = Token(lexema, "Palabra reservada", columna_inicio, linea)
                        estado = 1
                    elseif (trim(lexema) /= '') then
                        numErrores = numErrores + 1
                        print *, "lexema erroneo: ", lexema
                        errores(numErrores) = ErrorInfo(char, "Token no válido", columna_inicio, linea)
                        estado = 1
                    end if
                    
                    columna = columna + 1
                else
                    numErrores = numErrores + 1
                    columna = columna + 1
                    errores(numErrores) = ErrorInfo(char, "Caracter no perteneciente al alfabeto del lenguaje", columna, linea)
                end if
                puntero = puntero + 1

            case(1)
                if (any(char == A)) then
                    numErrores = numErrores + 1
                    errores(numErrores) = ErrorInfo(char, "Token no válido", columna, linea)
                    estado = 1
                    columna = columna + 1
                elseif (any(char == S)) then
                    lexema = ''
                    lexema = trim(lexema) // char
                    if (trim(lexema) == ':') then
                        numTokens = numTokens + 1
                        tokens(numTokens) = Token(lexema, "Dos puntos", columna, linea)
                        estado = 1
                    elseif (trim(lexema) == '{') then
                        numTokens = numTokens + 1
                        tokens(numTokens) = Token(lexema, "Llave de apertura", columna, linea)
                        lexema = ''
                        estado = 0
                    elseif (trim(lexema) == '"') then
                        estado = 2
                    elseif (trim(lexema) == ';') then
                        numTokens = numTokens + 1
                        tokens(numTokens) = Token(lexema, "Punto y coma", columna, linea)
                        lexema = ''
                        estado = 0
                    else 
                        numErrores = numErrores + 1
                        errores(numErrores) = ErrorInfo(char, "Token no válido", columna, linea)
                        estado = 1
                    end if
                    columna = columna + 1
                else
                    numErrores = numErrores + 1
                    errores(numErrores) = ErrorInfo(char, "Caracter no perteneciente al alfabeto del lenguaje", columna, linea)
                    columna = columna + 1
                    estado = 1
                end if
                puntero = puntero + 1
            case(2)
                lexema = trim(lexema) // char
                if (char == '"') then
                    numTokens = numTokens + 1
                    tokens(numTokens) = Token(lexema, "Cadena de texto", columna, linea)
                    print *, "lexema: ", lexema
                    estado = 1
                    columna = columna + 1
                else
                    columna = columna + 1
                end if
                puntero = puntero + 1
            case(3)
                if (any(char == A)) then
                    columna = columna + 1
                    estado = 3
                    lexema = trim(lexema) // char
                elseif (any(char == S)) then
                    columna = columna + 1
                    estado = 4
                    !almacenar token
                    lexema = ''
                else
                    numErrores = numErrores + 1
                    errores(numErrores) = ErrorInfo(char, "Caracter no perteneciente al alfabeto del lenguaje", columna, linea)
                    columna = columna + 1
                    estado = 3
                end if
                puntero = puntero + 1
            case(4)
                if (any(char == S)) then
                    columna = columna + 1
                    estado = 5
                end if
                puntero = puntero + 1
            end select
                    
        end if

    end do



    if (numErrores > 0) then
        call generar_html_errores(numErrores, errores)
      else
          print *, "No hay errores en el codigo."
    end if


contains

! Subrutina para convertir entero a cadena de texto
subroutine generar_html_errores(numErrores, errores)
    implicit none
    integer, intent(in) :: numErrores
    type(ErrorInfo), intent(in) :: errores(numErrores)
    character(len=100000) :: html_content
    character(len=100) :: str_descripcion, str_columna, str_linea,char_error

    integer :: file_unit, ios, i

    ! Si hay errores, se crea el archivo HTML
   if (numErrores > 0) then
! Abrir el archivo para escribir
        open(unit=file_unit, file="errores.html", status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            print *, "Error al crear el archivo HTML."
        else
            ! Escribir la cabecera del HTML directamente al archivo
            write(file_unit, '(A)') '<!DOCTYPE html>' // new_line('a')
            write(file_unit, '(A)') '<html><head><style>' // new_line('a')
            write(file_unit, '(A)') 'table { font-family: Arial, sans-serif;'
            write(file_unit, '(A)') 'border-collapse: collapse; width: 100%; }' // new_line('a')
            write(file_unit, '(A)') 'td, th { border: 1px solid #dddddd; text-align: left; padding: 8px; }' // new_line('a')
            write(file_unit, '(A)') 'tr:nth-child(even) { background-color: #f2f2f2; }' // new_line('a')
            write(file_unit, '(A)') '</style></head><body><h2>Tabla de Errores</h2>' // new_line('a')
            write(file_unit, '(A)') '<table><tr><th>Carácter</th><th>Descripcion' 
            write(file_unit, '(A)') '</th><th>Columna</th><th>Línea</th></tr>' // new_line('a')

            ! Bucle para formatear cada código ASCII y cada columna

            ! Bucle para agregar filas a la tabla
            do i = 1, numErrores
                write(str_descripcion, '(A)') trim(errores(i)%descripcion)
                write(str_columna, '(I0)') errores(i)%columna
                write(str_linea, '(I0)')  errores(i)%linea
                write(char_error, '(A)') trim(errores(i)%caracter)
     
                ! Escribir cada fila directamente al archivo

                write(file_unit, '(A)') '<tr><td>' // char_error // '</td><td>' // trim(str_descripcion) // & 
                '</td><td>' // trim(str_columna) // '</td><td>'&
                 // trim(str_linea) // '</td></tr>' // new_line('a')
            end do

            ! Cerrar la tabla y el HTML
            write(file_unit, '(A)') '</table></body></html>'
            close(file_unit)
        end if
    else
        print *, "No hay errores en el código."
    end if
end subroutine generar_html_errores

function itoa(num) result(str)
    implicit none
    integer, intent(in) :: num
    character(len=20) :: str

    write(str, '(I0)') num  ! Convierte el entero 'num' a cadena
end function itoa

end program main