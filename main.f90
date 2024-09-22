program main
    implicit none
    integer :: i, len, linea, columna, estado, puntero, numErrores, numTokens, numPaises, numPaises_continente, numContinentes, ios, espacio_texto, suma_saturacion
    character(len=10000) :: contenido, buffer
    character(len=10) :: str_codigo_ascii, str_columna, str_linea
    character(len=1) :: char
    character(len=100), allocatable :: lexema
    character(len=1), dimension(26) :: A
    character(len=1), dimension(6) :: S
    integer, dimension(10) :: N
    character(len=1) :: char_error
    character(len=50) :: nombre_continente, nombre_pais
    integer :: poblacion
    integer :: saturacion_pais_int, saturacion_continente
    character(len=4) :: saturacion_pais_str
    character(len=100) :: bandera
    logical :: is_pais, is_continente, is_nombre, is_poblacion, is_saturacion, is_bandera, nuevo_pais, nuevo_continente

    type :: ErrorInfo
        character(len=100) :: caracter  ! caracter
        character(len=50) :: descripcion  ! Descripción del error
        integer :: columna      ! Columna donde ocurrió el error
        integer :: linea        ! Línea donde ocurrió el error
    end type ErrorInfo

    type :: Token
        character(len=100) :: lexema
        character(len=25) :: tipo
        integer :: columna
        integer :: linea
    end type Token

    type :: Pais
        character(len=100) :: nombre_pais
        integer :: poblacion
        integer :: saturacion_pais_int
        character(len=100) :: bandera
    end type Pais

    type (Pais), allocatable, dimension(:) :: paises_continente

    type :: Continente
        character(len=100) :: nombre_continente
        type(Pais), allocatable, dimension(:) :: paises_continente
        integer :: saturacion_continente
    end type Continente

    type(ErrorInfo), dimension(1000) :: errores
    type(Token), dimension(5000) :: tokens
    type(Pais), dimension(250) :: paises
    type(Continente), dimension(250) :: continentes
    type(Pais) :: paisMenorSaturacion

    A = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
    S = [':', ';', '{', '}', '"', ' ']
    N = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    
    estado = 0
    puntero = 1
    columna = 1
    linea = 1
    numErrores = 0
    numTokens = 0
    numPaises = 0
    numPaises_continente = 0
    numContinentes = 0
    suma_saturacion = 0
    contenido = ''
    lexema = ''
    is_pais = .false.
    is_continente = .false.
    is_nombre = .false.
    is_poblacion = .false.
    is_saturacion = .false.
    is_bandera = .false.
    nuevo_pais = .false.
    nuevo_continente = .false.
    allocate(paises_continente(100))

    do
        read(*, '(A)', IOSTAT=ios) buffer
        if (ios /= 0) exit
        contenido = trim(contenido) // trim(buffer) // new_line('a') ! concatenamos el
        !contenido mas lo que viene en el buffer y como leemos por el salto de linea al final
    end do
    len = len_trim(contenido)
    do while(puntero <= len)
        char = contenido(puntero:puntero)
        if(ichar(char) == 10) then
            columna = 1 
            if (lexema /= '') then
                numErrores = numErrores + 1
                errores(numErrores) = ErrorInfo(lexema, "Token no valido", columna, linea)
                lexema = ''
            end if
            linea = linea + 1
            puntero = puntero + 1
        elseif(ichar(char) == 9) then
            columna = columna + 1
            puntero = puntero + 1
        elseif(ichar(char) == 32 .and. .not. is_nombre) then
            columna = columna + 1
            puntero = puntero + 1
        else
            select case (estado)
            case(0)
                if (any(char == A)) then
                    estado = 0
                    lexema = trim(lexema) // char
                    if (trim(lexema) == 'grafica') then
                        numTokens = numTokens + 1
                        tokens(numTokens) = Token(lexema, "Palabra reservada", columna-len_trim(lexema)+1, linea)
                        lexema = ''
                        estado = 1
                    elseif (trim(lexema) == 'nombre') then
                        numTokens = numTokens + 1
                        is_nombre = .true.
                        tokens(numTokens) = Token(lexema, "Palabra reservada", columna-len_trim(lexema)+1, linea)
                        lexema = ''
                        estado = 1
                    elseif (trim(lexema) == 'continente') then
                        numTokens = numTokens + 1
                        numContinentes = numContinentes + 1
                        is_continente = .true.
                        nuevo_continente = .true.
                        tokens(numTokens) = Token(lexema, "Palabra reservada", columna-len_trim(lexema)+1, linea)                    
                        lexema = ''
                        estado = 1
                    elseif (trim(lexema) == 'pais') then
                        numTokens = numTokens + 1
                        numPaises = numPaises + 1
                        numPaises_continente = numPaises_continente + 1
                        is_pais = .true.
                        nuevo_pais = .true.
                        tokens(numTokens) = Token(lexema, "Palabra reservada", columna-len_trim(lexema)+1, linea)
                        lexema = ''
                        estado = 1
                    elseif (trim(lexema) == 'poblacion') then
                        numTokens = numTokens + 1
                        is_poblacion = .true.
                        tokens(numTokens) = Token(lexema, "Palabra reservada", columna-len_trim(lexema)+1, linea)
                        lexema = ''
                        estado = 1
                    elseif (trim(lexema) == 'saturacion') then
                        numTokens = numTokens + 1
                        is_saturacion = .true.
                        tokens(numTokens) = Token(lexema, "Palabra reservada", columna-len_trim(lexema)+1, linea)
                        lexema = ''
                        estado = 1
                    elseif (trim(lexema) == 'bandera') then
                        numTokens = numTokens + 1
                        is_bandera = .true.
                        tokens(numTokens) = Token(lexema, "Palabra reservada", columna-len_trim(lexema)+1, linea)
                        lexema = ''
                        estado = 1
                    else
                        estado = 0
                    end if

                elseif (any(char == S)) then
                    if (trim(lexema) /= 'nombre' .and. trim(lexema) /= 'continente' .and. trim(lexema) /= 'pais' .and. trim(lexema) /= 'poblacion' .and. trim(lexema) /= 'saturacion' .and. trim(lexema) /= 'bandera') then
                        numErrores = numErrores + 1
                        errores(numErrores) = ErrorInfo(lexema, "Token no valido", columna, linea)
                        lexema = ''
                        lexema = trim(lexema) // char
                        numTokens = numTokens + 1
                        tokens(numTokens) = Token(lexema, "Dos puntos", columna, linea)
                        lexema = ''
                    end if
                    estado = 1
                else
                    numErrores = numErrores + 1
                    errores(numErrores) = ErrorInfo(char, "Caracter no perteneciente al alfabeto del lenguaje", columna, linea)
                    lexema = trim(lexema) // char
                    estado = 0
                end if
                columna = columna + 1
                puntero = puntero + 1
            case(1)
                if (any(char == A)) then
                    lexema = trim(lexema) // char
                    estado = 0
                elseif (any(char == S)) then
                    lexema = trim(lexema) // char
                    if (trim(lexema) == ':') then
                        numTokens = numTokens + 1
                        tokens(numTokens) = Token(lexema, "Dos puntos", columna, linea)
                        lexema = ''
                        if (is_poblacion .or. is_saturacion) then
                            estado = 3
                            is_poblacion = .false.
                        else
                            estado = 1
                        end if
                    elseif (trim(lexema) == '{') then
                        numTokens = numTokens + 1
                        tokens(numTokens) = Token(lexema, "Llave de apertura", columna, linea)
                        lexema = ''
                        estado = 1
                    elseif (trim(lexema) == '"') then
                         estado = 2
                    elseif (trim(lexema) == ';') then
                        numTokens = numTokens + 1
                        tokens(numTokens) = Token(lexema, "Punto y coma", columna, linea)
                        lexema = ''
                        estado = 1
                    elseif (trim(lexema) == ' ') then
                        lexema = ''
                        estado = 1
                    elseif (trim(lexema) == '}') then
                        numTokens = numTokens + 1
                        tokens(numTokens) = Token(lexema, "Llave de cierre", columna, linea)
                        if (nuevo_pais) then
                            paises_continente(numPaises_continente) = Pais(nombre_pais, poblacion, saturacion_pais_int, bandera)
                            paises(numPaises) = Pais(nombre_pais, poblacion, saturacion_pais_int, bandera)
                            nuevo_pais = .false.
                        elseif (nuevo_continente) then
                            do i=1, numPaises_continente
                                suma_saturacion = suma_saturacion + paises_continente(i)%saturacion_pais_int
                            end do
                            saturacion_continente = suma_saturacion / numPaises_continente
                            continentes(numContinentes) = Continente(nombre_continente, paises_continente, saturacion_continente)
                            deallocate(paises_continente)
                            allocate(paises_continente(100))
                            numPaises_continente = 0
                            nuevo_continente = .false.
                        end if
                        lexema = ''
                        estado = 1
                    else
                        numErrores = numErrores + 1
                        errores(numErrores) = ErrorInfo(char, "Token no válido", columna, linea)
                        estado = 1
                    end if
                elseif (any(ichar(char) - ichar('0') == N)) then
                    lexema = trim(lexema) // char
                    estado = 3
                else
                    numErrores = numErrores + 1
                    errores(numErrores) = ErrorInfo(char, "Caracter no perteneciente al alfabeto del lenguaje", columna, linea)
                    estado = 1
                end if
                columna = columna + 1
                puntero = puntero + 1
            case(2)
                lexema = trim(lexema) // char
                if (char == '"') then
                    numTokens = numTokens + 1
                    tokens(numTokens) = Token(lexema, "Cadena de texto", columna-len_trim(lexema), linea)
                    if (is_continente) then
                        nombre_continente = trim(lexema(2:len_trim(lexema)-1))
                        is_continente = .false.
                    else if (is_pais) then
                        nombre_pais = lexema(2:len_trim(lexema)-1)
                        is_pais = .false.
                    else if (is_bandera) then
                        bandera = lexema(2:len_trim(lexema)-1)
                        is_bandera = .false.
                    end if
                    lexema = ''
                    is_nombre = .false.
                    estado = 1
                end if
                columna = columna + 1
                puntero = puntero + 1
            case(3)
                if (any(ichar(char) - ichar('0') == N)) then
                    lexema = trim(lexema) // char
                elseif (char == '%') then
                    saturacion_pais_int = atoi(lexema)
                    lexema = trim(lexema) // char
                    numTokens = numTokens + 1
                    tokens(numTokens) = Token(lexema, "Porcentaje", columna, linea)
                    saturacion_pais_str = trim(lexema)
                    lexema = ''
                elseif (any(char == S)) then
                    if (is_saturacion) then
                        is_saturacion = .false.
                    else
                        numTokens = numTokens + 1
                        tokens(numTokens) = Token(lexema, "Numero", columna, linea)
                        poblacion = atoi(lexema)
                    
                    end if
                    lexema = ''
                    lexema = trim(lexema) // char
                    
                    if (trim(lexema) == ';') then
                        numTokens = numTokens + 1
                        tokens(numTokens) = Token(lexema, "Punto y coma", columna, linea)
                        lexema = ''
                        estado = 1
                    end if
                else
                    numErrores = numErrores + 1
                    errores(numErrores) = ErrorInfo(char, "Token no valido", columna, linea)
                    estado = 1
                end if
                columna = columna + 1
                puntero = puntero + 1
            end select

        end if
    end do
    if (numErrores > 0) then
        call generar_html_errores(numErrores, errores)
      else
        call generar_html_tokens(numTokens, tokens)
    end if
    
    paisMenorSaturacion = paises(1)
    do i=2, numPaises
        if (paises(i)%saturacion_pais_int < paisMenorSaturacion%saturacion_pais_int) then
            paisMenorSaturacion = paises(i)
        end if
    end do

    print *, trim(paisMenorSaturacion%nombre_pais)
    print *, trim(adjustl(itoa(paisMenorSaturacion%poblacion)))
    print *, trim(paisMenorSaturacion%bandera)

contains

subroutine generar_html_errores(numErrores, errores)
    implicit none
    integer, intent(in) :: numErrores
    type(ErrorInfo), intent(in) :: errores(numErrores)
    character(len=100000) :: html_content
    character(len=100) :: str_descripcion, str_columna, str_linea, str_numErrores, char_error
    integer :: file_unit, ios, i
    
    ! Abrir el archivo para escribir
    open(unit=file_unit, file="errores.html", status="replace", action="write", iostat=ios)
    if (ios /= 0) then
        print *, "Error al crear el archivo HTML."
    else
        ! Escribir la cabecera del HTML directamente al archivo
        write(file_unit, '(A)') '<!DOCTYPE html>' // new_line('a')
        write(file_unit, '(A)') '<html><head><title>Errores encontrados en el lenguaje</title><style>' // new_line('a')
        write(file_unit, '(A)') 'table { font-family: Arial, sans-serif;'
        write(file_unit, '(A)') 'border-collapse: collapse; width: 100%; }' // new_line('a')
        write(file_unit, '(A)') 'td, th { border: 1px solid #dddddd; text-align: left; padding: 8px; }' // new_line('a')
        write(file_unit, '(A)') 'tr:nth-child(even) { background-color: #f2f2f2; }' // new_line('a')
        write(file_unit, '(A)') '</style></head><body><h2>Errores encontrados en el lenguaje</h2>' // new_line('a')
        write(file_unit, '(A)') '<table><tr><th>No.</th><th>Error</th><th>Descripcion'
        write(file_unit, '(A)') '</th><th>Fila</th><th>Columna</th></tr>' // new_line('a')
        ! Bucle para formatear cada código ASCII y cada columna
        ! Bucle para agregar filas a la tabla
        do i = 1, numErrores
            write(str_numErrores, '(I0)') i
            write(str_descripcion, '(A)') trim(errores(i)%descripcion)
            write(str_columna, '(I0)') errores(i)%columna
            write(str_linea, '(I0)')  errores(i)%linea
            write(char_error, '(A)') trim(errores(i)%caracter)

            ! Escribir cada fila directamente al archivo
            write(file_unit, '(A)') '<tr><td>' // trim(str_numErrores) // '</td><td>' // char_error // '</td><td>' // trim(str_descripcion) // &
            '</td><td>' // trim(str_linea) // '</td><td>'&
                // trim(str_columna) // '</td></tr>' // new_line('a')
        end do
        ! Cerrar la tabla y el HTML
        write(file_unit, '(A)') '</table></body></html>'
        close(file_unit)
    end if

end subroutine generar_html_errores

subroutine generar_html_tokens(numTokens, tokens)
    implicit none
    integer, intent(in) :: numTokens
    type(Token), intent(in) :: tokens(numTokens)
    character(len=100000) :: html_content
    character(len=100) :: str_lexema, str_columna, str_linea, str_numTokens, char_token
    integer :: file_unit, ios, i
    
    ! Abrir el archivo para escribir
    open(unit=file_unit, file="tokens.html", status="replace", action="write", iostat=ios)
    if (ios /= 0) then

        print *, "Error al crear el archivo HTML."
        
    else
        ! Escribir la cabecera del HTML directamente al archivo
        write(file_unit, '(A)') '<!DOCTYPE html>' // new_line('a')
        write(file_unit, '(A)') '<html><head><title>Tokens encontrados en el lenguaje</title><style>' // new_line('a')
        write(file_unit, '(A)') 'table { font-family: Arial, sans-serif;'
        write(file_unit, '(A)') 'border-collapse: collapse; width: 100%; }' // new_line('a')
        write(file_unit, '(A)') 'td, th { border: 1px solid #dddddd; text-align: left; padding: 8px; }' // new_line('a')
        write(file_unit, '(A)') 'tr:nth-child(even) { background-color: #f2f2f2; }' // new_line('a')
        write(file_unit, '(A)') '</style></head><body><h2>Tokens encontrados en el lenguaje</h2>' // new_line('a')
        write(file_unit, '(A)') '<table><tr><th>No.</th><th>Lexema</th><th>Tipo'
        write(file_unit, '(A)') '</th><th>Fila</th><th>Columna</th></tr>' // new_line('a')
        ! Bucle para formatear cada código ASCII y cada columna
        ! Bucle para agregar filas a la tabla
        do i = 1, numTokens
            write(str_numTokens, '(I0)') i
            write(str_lexema, '(A)') trim(tokens(i)%lexema)
            write(str_columna, '(I0)') tokens(i)%columna
            write(str_linea, '(I0)')  tokens(i)%linea
            write(char_token, '(A)') trim(tokens(i)%tipo)

            ! Escribir cada fila directamente al archivo
            write(file_unit, '(A)') '<tr><td>' // trim(str_numTokens) // '</td><td>' // char_token // '</td><td>' // trim(str_lexema) // &
            '</td><td>' // trim(str_linea) // '</td><td>'&
                // trim(str_columna) // '</td></tr>' // new_line('a')
        end do
        ! Cerrar la tabla y el HTML
        write(file_unit, '(A)') '</table></body></html>'
        close(file_unit)
    end if

end subroutine generar_html_tokens

function itoa(num) result(str)
    implicit none
    integer, intent(in) :: num
    character(len=20) :: str
    write(str, '(I0)') num  ! Convierte el entero 'num' a cadena
end function itoa

function atoi(str) result(num)
    implicit none
    character(len=*), intent(in) :: str
    integer :: num
    read(str, '(I20)') num  ! Convierte la cadena 'str' a entero
end function atoi

end program main