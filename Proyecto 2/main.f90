program analizador_lexico
    use error
    use token

    implicit none
    integer :: len, fila, columna, estado, puntero
    integer :: ios, unidad
    character(len=100000) :: contenido, buffer
    character(len=1) :: char
    character(len=100) :: aux_tkn

    estado = 0
    puntero = 1
    columna = 0
    fila = 1
    aux_tkn = ""

    contenido = ""

    unidad = 10
    open(unit=unidad, file='archivo.txt', status='old', action='read', iostat=ios)

    if (ios /= 0) then
        print *, 'Error al abrir el archivo'
        stop
    end if

    do
        read(unidad, '(A)', iostat=ios) buffer
        if (ios /= 0) exit  ! Salir del bucle al llegar al final del archivo
        contenido = trim(contenido) // trim(buffer) // new_line('a')
    end do

    close(unidad)

    len = len_trim(contenido)

    do while(puntero <= len)
        char = contenido(puntero:puntero)
        select case (estado)
            case (0)
                
                ! Verifica que el carácter sea una simbolo
                if (char == ';' .or. char == '-' .or. char == '.' .or. char == '(' .or. char == ')' .or. char == ',' .or. char == '<' .or. char == '>' .or. char == '!') then
                    estado = 1
                    columna = columna + 1
                elseif ( char >= 'A' .and. char <= 'Z' .or. (char >= 'a' .and. char <= 'z') ) then
                    estado = 2
                
                elseif (char >= '0' .and. char <= '9' ) then
                    estado = 3

                elseif (char == '"') then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1 
                    estado = 4       

                elseif (ichar(char) == 10) then ! Actualizo la posicion
                    ! Salto de línea
                    columna = 0
                    fila = fila + 1
                    puntero = puntero + 1
                elseif (ichar(char) == 9) then
                    ! Tabulación
                    columna = columna + 4
                    puntero = puntero + 1
                elseif (ichar(char) == 32) then
                    ! Espacio en blanco
                    columna = columna + 1
                    puntero = puntero + 1  
            
                else
                    ! Reporta un error si el carácter no es válido
                    ! CALL agregar_error(char, 'Error Lexico', fila, columna)
                    columna = columna + 1
                    puntero = puntero + 1 

                end if
                
            case (1)
                if ( char == ';' ) then
                    call agregar_token(char, 'tk_pyc', fila, columna)
                    
                elseif ( char == '.' ) then
                    call agregar_token(char, 'tk_punto', fila, columna)

                elseif ( char == ',' ) then
                    call agregar_token(char, 'tk_coma', fila, columna)

                elseif ( char == '>') then
                    call agregar_token(char, 'tk_mayor', fila, columna)

                elseif ( char == '<') then
                    call agregar_token(char, 'tk_menor', fila, columna)

                elseif ( char == '(') then
                    call agregar_token(char, 'tk_par_izq', fila, columna)

                elseif ( char == ')') then
                    call agregar_token(char, 'tk_par_der', fila, columna)         
                
                elseif ( char == '-') then
                    call agregar_token(char, 'tk_guion', fila, columna)
                
                elseif ( char == '!') then
                    call agregar_token(char, 'tk_exp', fila, columna) 

                end if
                puntero = puntero + 1
                estado = 0

            case (2)
                if ( (char >= 'A' .and. char <= 'Z') .or. (char >= 'a' .and. char <= 'z') .or. (char >= '0' .and. char <= '9' ) ) then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1
                    
                else
                    if ((aux_tkn == 'Contenedor')) then
                        call agregar_token(aux_tkn, 'tk_contenedor', fila, columna)

                    elseif ((aux_tkn == 'Etiqueta')) then
                        call agregar_token(aux_tkn, 'tk_etiqueta', fila, columna)
                    
                    elseif ((aux_tkn == 'Boton')) then
                        call agregar_token(aux_tkn, 'tk_boton', fila, columna)
                    
                    elseif ((aux_tkn == 'setAncho')) then
                        call agregar_token(aux_tkn, 'tk_setAncho', fila, columna)
                    
                    elseif ((aux_tkn == 'setAlto')) then
                        call agregar_token(aux_tkn, 'tk_setAlto', fila, columna)
                    
                    elseif ((aux_tkn == 'setColorFondo')) then
                        call agregar_token(aux_tkn, 'tk_setColorFondo', fila, columna)

                    elseif ((aux_tkn == 'setColorLetra')) then
                        call agregar_token(aux_tkn, 'tk_setColorLetra', fila, columna)
                    
                    elseif ((aux_tkn == 'setTexto')) then
                        call agregar_token(aux_tkn, 'tk_setTexto', fila, columna)

                    elseif ((aux_tkn == 'setPosicion')) then
                        call agregar_token(aux_tkn, 'tk_setPosicion', fila, columna)
                    
                    elseif (aux_tkn == 'this') then
                        call agregar_token(aux_tkn, 'tk_this', fila, columna)
                    
                    elseif (aux_tkn == 'add') then
                        call agregar_token(aux_tkn, 'tk_add', fila, columna)

                    else 
                        call agregar_token(aux_tkn, 'tk_id', fila, columna)

                    end if

                    aux_tkn = ""
                    estado = 0      
                        
                end if

            case (3)
                if (char >= '0' .and. char <= '9' ) then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1
                    
                else
                    call agregar_token(aux_tkn, 'tk_num', fila, columna)
                
                    aux_tkn = ""
                    estado = 0
                end if

            case (4)
                if (ichar(char) >= 0 .and. ichar(char) <= 255 .and. char .ne. '"') then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1 

                    estado = 6

                else if ( char == '"') then
                    
                    estado = 5

                else
                    ! call agregar_error(aux_tkn, 'Error lexico', fila, columna)

                    aux_tkn = ""
                    estado = 0
                
                end if

            case (5)
                
                aux_tkn = trim(aux_tkn) // char
                columna = columna + 1
                puntero = puntero + 1

                call agregar_token(aux_tkn, 'tk_literal', fila, columna)
                
                aux_tkn = ""
                estado = 0
                    
            case (6)
                if (ichar(char) >= 0 .and. ichar(char) <= 255 .and. char .ne. '"') then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1 

                elseif ( char == '"') then
                    estado = 5
                else
                    ! call agregar_error(aux_tkn, 'Error lexico', fila, columna)

                    aux_tkn = ""
                    estado = 0
                end if             

        end select
    end do
    
    call parser
    
    call imprimir_errores
    
    ! call imprimir_tokens

    call imprimir_etiquetas
    
    
        
end program analizador_lexico