MODULE error
    implicit none

    type :: Err
        character(len=10) :: tipo
        integer :: fila
        integer:: columna
        CHARACTER(LEN = 100) :: ultimo_token
        CHARACTER(LEN = 100) :: descripcion
        
    End type Err

    ! Declaración de un arreglo de Err para almacenar los errores
    type(Err), ALLOCATABLE ::  error_array(:)

contains 

    ! Subrutina para agregar errores a la lista de error
    subroutine agregar_error(tipo, fila, columna, ultimo_token, descripcion)
        CHARACTER(LEN=*), INTENT(IN) :: tipo
        integer :: fila, columna
        CHARACTER(LEN=*), INTENT(IN) :: ultimo_token
        CHARACTER(LEN=*), INTENT(IN) :: descripcion

        type(Err) :: nuevo_error
        integer :: n
        type(Err), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo error
        nuevo_error%tipo = tipo
        nuevo_error%fila = fila
        nuevo_error%columna = columna
        nuevo_error%ultimo_token = ultimo_token
        nuevo_error%descripcion = descripcion

        ! Agrego el nuevo error a la lista de errores
        if (.NOT. ALLOCATED(error_array)) then !Si esta vacia
            ALLOCATE(error_array(1)) ! Se le asigna memoria para un error de la lista
            error_array(1) =  nuevo_error !Se convierte en el error nuevo
        else
            n = size(error_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = error_array !Reservo memoria
            temp_array(n+1) = nuevo_error
            DEALLOCATE(error_array) !Libero memoria
            ALLOCATE(error_array(n+1)) !Reservo memoria de nuevo
            error_array = temp_array
        end if
    end subroutine agregar_error

    subroutine generar_json_errores()
        implicit none
        integer :: i ! Contador del bucle
        character(len=:), allocatable :: json_string
        character(len=20) :: str_fila, str_columna
        integer :: unit_number
        character(len=*), parameter :: RUTA_ARCHIVO = 'Archivos/errores.json'  ! Nueva constante para la ruta
    
        if (.NOT. ALLOCATED(error_array)) then
            print *, "No hay errores"
        else
            ! Inicializar la cadena JSON
            json_string = '{"errores": ['
    
            do i = 1, size(error_array)
                write(str_fila, '(I0)') error_array(i)%fila
                write(str_columna, '(I0)') error_array(i)%columna
    
                ! Agregar cada error al JSON
                json_string = trim(json_string) // &
                    '{"tipo": "' // trim(error_array(i)%tipo) // '", ' // &
                    '"fila": ' // trim(str_fila) // ', ' // &
                    '"columna": ' // trim(str_columna) // ', ' // &
                    '"ultimo_token": "' // trim(error_array(i)%ultimo_token) // '", ' // &
                    '"descripcion": "' // trim(error_array(i)%descripcion) // '"}'
    
                if (i < size(error_array)) then
                    json_string = trim(json_string) // ', '
                end if
            end do
    
            ! Cerrar el array y el objeto JSON
            json_string = trim(json_string) // ']}'
    
            ! Escribir la cadena JSON en un archivo en la carpeta Archivos
            open(newunit=unit_number, file=RUTA_ARCHIVO, status='replace', action='write', iostat=i)
            if (i /= 0) then
                print *, "Error al abrir el archivo para escribir en la carpeta Archivos"
                return
            end if
    
            write(unit_number, '(A)') trim(json_string)
            close(unit_number)
        end if
    end subroutine generar_json_errores
    
END MODULE error



MODULE etiqueta
    implicit none

    type :: Tag
        CHARACTER(LEN = 50) :: id
        CHARACTER(LEN = 20) :: alto
        CHARACTER(LEN = 20) :: ancho
        CHARACTER(LEN = 200) :: texto
        CHARACTER(LEN = 50) :: color_texto_r
        CHARACTER(LEN = 50) :: color_texto_g
        CHARACTER(LEN = 50) :: color_texto_b
        CHARACTER(LEN = 50) :: color_fondo_r
        CHARACTER(LEN = 50) :: color_fondo_g
        CHARACTER(LEN = 50) :: color_fondo_b
        CHARACTER(LEN = 50) :: posicion_x
        CHARACTER(LEN = 50) :: posicion_y
    End type Tag

    ! Declaración de un arreglo de Tag para almacenar los etiquetas
    type(Tag), ALLOCATABLE ::  etiqueta_array(:)
    

contains

    ! Subrutina para agregar etiquetas a la lista de etiqueta
    subroutine agregar_etiqueta(id)
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(Tag) :: nuevo_etiqueta
        integer :: n
        type(Tag), ALLOCATABLE ::  temp_array_etiqueta(:)
        
        !Inicializo los datos del nuevo etiqueta
        nuevo_etiqueta%id = id
        nuevo_etiqueta%alto = ""
        nuevo_etiqueta%ancho = ""
        nuevo_etiqueta%texto = ""
        nuevo_etiqueta%color_texto_r = ""
        nuevo_etiqueta%color_texto_g = ""
        nuevo_etiqueta%color_texto_b = ""
        nuevo_etiqueta%color_fondo_r = ""
        nuevo_etiqueta%color_fondo_g = ""
        nuevo_etiqueta%color_fondo_b = ""
        nuevo_etiqueta%posicion_x = ""
        nuevo_etiqueta%posicion_y = ""


        ! Agrego el nuevo etiqueta a la lista de etiquetas
        if (.NOT. ALLOCATED(etiqueta_array)) then !Si esta vacia
            ALLOCATE(etiqueta_array(1)) ! Se le asigna memoria para un etiqueta de la lista
            etiqueta_array(1) =  nuevo_etiqueta !Se convierte en el etiqueta nuevo
        else
            n = size(etiqueta_array)
            ALLOCATE(temp_array_etiqueta(n+1))
            temp_array_etiqueta(:n) = etiqueta_array !Reservo memoria
            temp_array_etiqueta(n+1) = nuevo_etiqueta
            DEALLOCATE(etiqueta_array) !Libero memoria
            ALLOCATE(etiqueta_array(n+1)) !Reservo memoria de nuevo
            etiqueta_array = temp_array_etiqueta
        end if
    end subroutine agregar_etiqueta

    subroutine imprimir_etiquetas()

    integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            print *, "etiquetas encontrados: ", size(etiqueta_array)
            DO i = 1, size(etiqueta_array)
                print *, 'id: ', trim(etiqueta_array(i)%id)
                print *, 'alto: ', trim(etiqueta_array(i)%alto)
                print *, 'ancho: ', trim(etiqueta_array(i)%ancho)
                print *, 'texto: ', trim(etiqueta_array(i)%texto)
                print *, 'color_texto_r: ', trim(etiqueta_array(i)%color_texto_r)
                print *, 'color_texto_g: ', trim(etiqueta_array(i)%color_texto_g)
                print *, 'color_texto_b: ', trim(etiqueta_array(i)%color_texto_b)
                print *, 'posicion_x: ', trim(etiqueta_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(etiqueta_array(i)%posicion_y)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_etiquetas

    
    subroutine etiqueta_set_alto(id, alto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: alto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%alto = alto
                end if
            END DO
        end if

    end subroutine etiqueta_set_alto

    subroutine etiqueta_set_ancho(id, ancho)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: ancho
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%ancho = ancho
                end if
            END DO
        end if

    end subroutine etiqueta_set_ancho

    subroutine etiqueta_set_texto(id, texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%texto = texto
                end if
            END DO
        end if

    end subroutine etiqueta_set_texto

    subroutine etiqueta_set_color_texto(id, color_texto_r, color_texto_g, color_texto_b)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: color_texto_r
        CHARACTER(LEN=*), INTENT(IN) :: color_texto_g
        CHARACTER(LEN=*), INTENT(IN) :: color_texto_b
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%color_texto_r = color_texto_r
                    etiqueta_array(i)%color_texto_g = color_texto_g
                    etiqueta_array(i)%color_texto_b = color_texto_b
                end if
            END DO
        end if

    end subroutine etiqueta_set_color_texto

    subroutine etiqueta_set_color_fondo(id, color_fondo_r, color_fondo_g, color_fondo_b)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: color_fondo_r
        CHARACTER(LEN=*), INTENT(IN) :: color_fondo_g
        CHARACTER(LEN=*), INTENT(IN) :: color_fondo_b
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%color_fondo_r = color_fondo_r
                    etiqueta_array(i)%color_fondo_g = color_fondo_g
                    etiqueta_array(i)%color_fondo_b = color_fondo_b
                end if
            END DO
        end if

    end subroutine etiqueta_set_color_fondo

    subroutine etiqueta_set_posicion(id, posicion_x, posicion_y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: posicion_x
        CHARACTER(LEN=*), INTENT(IN) :: posicion_y
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == id) then
                    etiqueta_array(i)%posicion_x = posicion_x
                    etiqueta_array(i)%posicion_y = posicion_y
                end if
            END DO
        end if

    end subroutine etiqueta_set_posicion

    ! Función para buscar una etiqueta por su ID
    FUNCTION buscar_etiqueta_por_id(id) RESULT(encontrado)
        CHARACTER(LEN=*), INTENT(IN) :: id
        logical :: encontrado
        integer :: i

        encontrado = .FALSE.

        if (.NOT. ALLOCATED(etiqueta_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(etiqueta_array)
                if (trim(etiqueta_array(i)%id) == trim(id)) then
                    encontrado = .TRUE.
                    return
                end if
            END DO
        end if
    END FUNCTION buscar_etiqueta_por_id

END MODULE etiqueta



MODULE boton
    implicit none

    type :: Button
        CHARACTER(LEN = 50) :: id
        CHARACTER(LEN = 20) :: alto
        CHARACTER(LEN = 20) :: ancho
        CHARACTER(LEN = 200) :: texto
        CHARACTER(LEN = 20) :: alineacion
        CHARACTER(LEN = 50) :: posicion_x
        CHARACTER(LEN = 50) :: posicion_y
    End type Button

    ! Declaración de un arreglo de Tag para almacenar los etiquetas
    type(Button), ALLOCATABLE ::  boton_array(:)

contains

    ! Subrutina para agregar botones a la lista de botones
    subroutine agregar_boton(id)
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(Button) :: nuevo_boton
        integer :: n
        type(Button), ALLOCATABLE ::  temp_array_boton(:)
        
        !Inicializo los datos del nuevo boton
        nuevo_boton%id = id
        nuevo_boton%alto = "25"
        nuevo_boton%ancho = "100"
        nuevo_boton%texto = ""
        nuevo_boton%alineacion = "izquierda"
        nuevo_boton%posicion_x = ""
        nuevo_boton%posicion_y = ""

        ! Agrego el nuevo boton a la lista de botones
        if (.NOT. ALLOCATED(boton_array)) then !Si esta vacia
            ALLOCATE(boton_array(1)) ! Se le asigna memoria para un boton de la lista
            boton_array(1) =  nuevo_boton !Se convierte en el boton nuevo
        else
            n = size(boton_array)
            ALLOCATE(temp_array_boton(n+1))
            temp_array_boton(:n) = boton_array !Reservo memoria
            temp_array_boton(n+1) = nuevo_boton
            DEALLOCATE(boton_array) !Libero memoria
            ALLOCATE(boton_array(n+1)) !Reservo memoria de nuevo
            boton_array = temp_array_boton
        end if
    end subroutine agregar_boton

    subroutine imprimir_botones()

    integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(boton_array)) then
            print *, "No hay botones"
        else
            print *, "botones encontrados: ", size(boton_array)
            DO i = 1, size(boton_array)
                print *, 'id: ', trim(boton_array(i)%id)
                print *, 'alto: ', trim(boton_array(i)%alto)
                print *, 'ancho: ', trim(boton_array(i)%ancho)
                print *, 'texto: ', trim(boton_array(i)%texto)
                print *, 'alineacion: ', trim(boton_array(i)%alineacion)
                print *, 'posicion_x: ', trim(boton_array(i)%posicion_x)
                print *, 'posicion_y: ',trim(boton_array(i)%posicion_y)
                print *, '---------------------------------'
            END DO
        end if
    
    end subroutine imprimir_botones

    subroutine boton_set_texto(id, texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(boton_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(boton_array)
                if (trim(boton_array(i)%id) == id) then
                    boton_array(i)%texto = texto
                end if
            END DO
        end if

    end subroutine boton_set_texto

    subroutine boton_set_alineacion(id, alineacion)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: alineacion
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(boton_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(boton_array)
                if (trim(boton_array(i)%id) == id) then
                    boton_array(i)%alineacion = alineacion
                end if
            END DO
        end if

    end subroutine boton_set_alineacion

    subroutine boton_set_posicion(id, posicion_x, posicion_y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: posicion_x
        CHARACTER(LEN=*), INTENT(IN) :: posicion_y
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(boton_array)) then
            print *, "No hay etiquetas"
        else
            DO i = 1, size(boton_array)
                if (trim(boton_array(i)%id) == id) then
                    boton_array(i)%posicion_x = posicion_x
                    boton_array(i)%posicion_y = posicion_y
                end if
            END DO
        end if

    end subroutine boton_set_posicion

    ! Función para buscar un boton por su ID
    FUNCTION buscar_boton_por_id(id) RESULT(encontrado)
        CHARACTER(LEN=*), INTENT(IN) :: id
        logical :: encontrado
        integer :: i

        encontrado = .FALSE.

        if (.NOT. ALLOCATED(boton_array)) then
            print *, "No hay botones"
        else
            DO i = 1, size(boton_array)
                if (trim(boton_array(i)%id) == trim(id)) then
                    encontrado = .TRUE.
                    return
                end if
            END DO
        end if
    END FUNCTION buscar_boton_por_id
    
END MODULE boton



MODULE check
implicit none

    type :: CheckBox
        CHARACTER(LEN = 50) :: id
        CHARACTER(LEN = 20) :: alto
        CHARACTER(LEN = 20) :: ancho
        CHARACTER(LEN = 200) :: texto
        CHARACTER(LEN = 10) :: marcado
        CHARACTER(LEN = 50) :: grupo
        CHARACTER(LEN = 50) :: posicion_x
        CHARACTER(LEN = 50) :: posicion_y
    End type CheckBox

    ! Declaración de un arreglo de Tag para almacenar los etiquetas
    type(CheckBox), ALLOCATABLE ::  check_array(:)

contains

    subroutine agregar_check(id)
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(CheckBox) :: nuevo_check
        integer :: n
        type(CheckBox), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo check
        nuevo_check%id = id
        nuevo_check%alto = "25"
        nuevo_check%ancho = "100"
        nuevo_check%texto = ""
        nuevo_check%marcado = "false"
        nuevo_check%grupo = ""
        nuevo_check%posicion_x = ""
        nuevo_check%posicion_y = ""

        ! Agrego el nuevo check a la lista de checks
        if (.NOT. ALLOCATED(check_array)) then !Si esta vacia
            ALLOCATE(check_array(1)) ! Se le asigna memoria para un check de la lista
            check_array(1) =  nuevo_check !Se convierte en el check nuevo
        else
            n = size(check_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = check_array !Reservo memoria
            temp_array(n+1) = nuevo_check
            DEALLOCATE(check_array) !Libero memoria
            ALLOCATE(check_array(n+1)) !Reservo memoria de nuevo
            check_array = temp_array
        end if
    end subroutine agregar_check

    subroutine imprimir_checks()
        
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(check_array)) then
            print *, "No hay checks"
        else
            print *, "checks encontrados: ", size(check_array)
            DO i = 1, size(check_array)
                print *, 'id: ', trim(check_array(i)%id)
                print *, 'alto: ', trim(check_array(i)%alto)
                print *, 'ancho: ', trim(check_array(i)%ancho)
                print *, 'texto: ', trim(check_array(i)%texto)
                print *, 'marcado: ', check_array(i)%marcado
                print *, 'grupo: ', check_array(i)%grupo
                print *, 'posicion_x: ', trim(check_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(check_array(i)%posicion_y)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_checks

    subroutine check_set_texto(id, texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(check_array)) then
            print *, "No hay checks"
        else
            DO i = 1, size(check_array)
                if (trim(check_array(i)%id) == id) then
                    check_array(i)%texto = texto
                end if
            END DO
        end if

    end subroutine check_set_texto

    subroutine check_set_marcado(id, marcado)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: marcado
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(check_array)) then
            print *, "No hay checks"
        else
            DO i = 1, size(check_array)
                if (trim(check_array(i)%id) == id) then
                    check_array(i)%marcado = marcado
                end if
            END DO
        end if

    end subroutine check_set_marcado

    subroutine check_set_grupo(id, grupo)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: grupo
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(check_array)) then
            print *, "No hay checks"
        else
            DO i = 1, size(check_array)
                if (trim(check_array(i)%id) == id) then
                    check_array(i)%grupo = grupo
                end if
            END DO
        end if

    end subroutine check_set_grupo

    subroutine check_set_posicion(id, posicion_x, posicion_y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: posicion_x
        CHARACTER(LEN=*), INTENT(IN) :: posicion_y
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(check_array)) then
            print *, "No hay checks"
        else
            DO i = 1, size(check_array)
                if (trim(check_array(i)%id) == id) then
                    check_array(i)%posicion_x = posicion_x
                    check_array(i)%posicion_y = posicion_y
                end if
            END DO
        end if

    end subroutine check_set_posicion

    ! Función para buscar un check por su ID
    FUNCTION buscar_check_por_id(id) RESULT(encontrado)
        CHARACTER(LEN=*), INTENT(IN) :: id
        logical :: encontrado
        integer :: i

        encontrado = .FALSE.

        if (.NOT. ALLOCATED(check_array)) then
            print *, "No hay checks"
        else
            DO i = 1, size(check_array)
                if (trim(check_array(i)%id) == trim(id)) then
                    encontrado = .TRUE.
                    return
                end if
            END DO
        end if
    END FUNCTION buscar_check_por_id

END MODULE check



MODULE radioBoton
implicit none

    type :: RadioButton
        CHARACTER(LEN = 50) :: id
        CHARACTER(LEN = 20) :: alto
        CHARACTER(LEN = 20) :: ancho
        CHARACTER(LEN = 200) :: texto
        CHARACTER(LEN = 5) :: marcado
        character(LEN = 50) :: grupo
        CHARACTER(LEN = 50) :: posicion_x
        CHARACTER(LEN = 50) :: posicion_y
    End type RadioButton

    ! Declaración de un arreglo de Tag para almacenar los etiquetas
    type(RadioButton), ALLOCATABLE ::  radioBoton_array(:)

contains

    subroutine agregar_radioBoton(id)
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(RadioButton) :: nuevo_radioBoton
        integer :: n
        type(RadioButton), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo radioBoton
        nuevo_radioBoton%id = id
        nuevo_radioBoton%alto = "25"
        nuevo_radioBoton%ancho = "100"
        nuevo_radioBoton%texto = ""
        nuevo_radioBoton%marcado = ""
        nuevo_radioBoton%grupo = ""
        nuevo_radioBoton%posicion_x = ""
        nuevo_radioBoton%posicion_y = ""

        ! Agrego el nuevo radioBoton a la lista de radioBotones
        if (.NOT. ALLOCATED(radioBoton_array)) then !Si esta vacia
            ALLOCATE(radioBoton_array(1)) ! Se le asigna memoria para un radioBoton de la lista
            radioBoton_array(1) =  nuevo_radioBoton !Se convierte en el radioBoton nuevo
        else
            n = size(radioBoton_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = radioBoton_array !Reservo memoria
            temp_array(n+1) = nuevo_radioBoton
            DEALLOCATE(radioBoton_array) !Libero memoria
            ALLOCATE(radioBoton_array(n+1)) !Reservo memoria de nuevo
            radioBoton_array = temp_array
        end if
    end subroutine agregar_radioBoton

    subroutine imprimir_radioBotones()
        
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(radioBoton_array)) then
            print *, "No hay radioBotones"
        else
            print *, "radioBotones encontrados: ", size(radioBoton_array)
            DO i = 1, size(radioBoton_array)
                print *, 'id: ', trim(radioBoton_array(i)%id)
                print *, 'alto: ', trim(radioBoton_array(i)%alto)
                print *, 'ancho: ', trim(radioBoton_array(i)%ancho)
                print *, 'texto: ', trim(radioBoton_array(i)%texto)
                print *, 'marcado: ', radioBoton_array(i)%marcado
                print *, 'grupo: ', radioBoton_array(i)%grupo
                print *, 'posicion_x: ', trim(radioBoton_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(radioBoton_array(i)%posicion_y)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_radioBotones

    subroutine radioBoton_set_texto(id, texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(radioBoton_array)) then
            print *, "No hay radioBotones"
        else
            DO i = 1, size(radioBoton_array)
                if (trim(radioBoton_array(i)%id) == id) then
                    radioBoton_array(i)%texto = texto
                end if
            END DO
        end if

    end subroutine radioBoton_set_texto

    subroutine radioBoton_set_marcado(id, marcado)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: marcado
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(radioBoton_array)) then
            print *, "No hay radioBotones"
        else
            DO i = 1, size(radioBoton_array)
                if (trim(radioBoton_array(i)%id) == id) then
                    radioBoton_array(i)%marcado = marcado
                end if
            END DO
        end if

    end subroutine radioBoton_set_marcado

    subroutine radioBoton_set_grupo(id, grupo)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: grupo
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(radioBoton_array)) then
            print *, "No hay radioBotones"
        else
            DO i = 1, size(radioBoton_array)
                if (trim(radioBoton_array(i)%id) == id) then
                    radioBoton_array(i)%grupo = grupo
                end if
            END DO
        end if

    end subroutine radioBoton_set_grupo

    subroutine radioBoton_set_posicion(id, posicion_x, posicion_y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: posicion_x
        CHARACTER(LEN=*), INTENT(IN) :: posicion_y
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(radioBoton_array)) then
            print *, "No hay radioBotones"
        else
            DO i = 1, size(radioBoton_array)
                if (trim(radioBoton_array(i)%id) == id) then
                    radioBoton_array(i)%posicion_x = posicion_x
                    radioBoton_array(i)%posicion_y = posicion_y
                end if
            END DO
        end if

    end subroutine radioBoton_set_posicion

    ! Función para buscar un radioBoton por su ID
    FUNCTION buscar_radioBoton_por_id(id) RESULT(encontrado)
        CHARACTER(LEN=*), INTENT(IN) :: id
        logical :: encontrado
        integer :: i

        encontrado = .FALSE.

        if (.NOT. ALLOCATED(radioBoton_array)) then
            print *, "No hay radioBotones"
        else
            DO i = 1, size(radioBoton_array)
                if (trim(radioBoton_array(i)%id) == trim(id)) then
                    encontrado = .TRUE.
                    return
                end if
            END DO
        end if
    END FUNCTION buscar_radioBoton_por_id

END MODULE radioBoton



MODULE texto
implicit none

    type :: Text
        CHARACTER(LEN = 50) :: id
        CHARACTER(LEN = 20) :: alto
        CHARACTER(LEN = 20) :: ancho
        CHARACTER(LEN = 200) :: texto
        CHARACTER(LEN = 20) :: alineacion
        CHARACTER(LEN = 50) :: posicion_x
        CHARACTER(LEN = 50) :: posicion_y
    End type Text

    ! Declaración de un arreglo de Tag para almacenar los etiquetas
    type(Text), ALLOCATABLE ::  texto_array(:)

contains

    subroutine agregar_texto(id)
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(Text) :: nuevo_texto
        integer :: n
        type(Text), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo texto
        nuevo_texto%id = id
        nuevo_texto%alto = "25"
        nuevo_texto%ancho = "100"
        nuevo_texto%texto = ""
        nuevo_texto%alineacion = "izquierda"
        nuevo_texto%posicion_x = ""
        nuevo_texto%posicion_y = ""

        ! Agrego el nuevo texto a la lista de textos
        if (.NOT. ALLOCATED(texto_array)) then !Si esta vacia
            ALLOCATE(texto_array(1)) ! Se le asigna memoria para un texto de la lista
            texto_array(1) =  nuevo_texto !Se convierte en el texto nuevo
        else
            n = size(texto_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = texto_array !Reservo memoria
            temp_array(n+1) = nuevo_texto
            DEALLOCATE(texto_array) !Libero memoria
            ALLOCATE(texto_array(n+1)) !Reservo memoria de nuevo
            texto_array = temp_array
        end if
    end subroutine agregar_texto

    subroutine imprimir_textos()
        
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(texto_array)) then
            print *, "No hay textos"
        else
            print *, "textos encontrados: ", size(texto_array)
            DO i = 1, size(texto_array)
                print *, 'id: ', trim(texto_array(i)%id)
                print *, 'alto: ', trim(texto_array(i)%alto)
                print *, 'ancho: ', trim(texto_array(i)%ancho)
                print *, 'texto: ', trim(texto_array(i)%texto)
                print *, 'alineacion: ', trim(texto_array(i)%alineacion)
                print *, 'posicion_x: ', trim(texto_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(texto_array(i)%posicion_y)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_textos

    subroutine texto_set_texto(id, texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(texto_array)) then
            print *, "No hay textos"
        else
            DO i = 1, size(texto_array)
                if (trim(texto_array(i)%id) == id) then
                    texto_array(i)%texto = texto
                end if
            END DO
        end if

    end subroutine texto_set_texto

    subroutine texto_set_alineacion(id, alineacion)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: alineacion
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(texto_array)) then
            print *, "No hay textos"
        else
            DO i = 1, size(texto_array)
                if (trim(texto_array(i)%id) == id) then
                    texto_array(i)%alineacion = alineacion
                end if
            END DO
        end if

    end subroutine texto_set_alineacion

    subroutine texto_set_posicion(id, posicion_x, posicion_y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: posicion_x
        CHARACTER(LEN=*), INTENT(IN) :: posicion_y
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(texto_array)) then
            print *, "No hay textos"
        else
            DO i = 1, size(texto_array)
                if (trim(texto_array(i)%id) == id) then
                    texto_array(i)%posicion_x = posicion_x
                    texto_array(i)%posicion_y = posicion_y
                end if
            END DO
        end if

    end subroutine texto_set_posicion

    ! Función para buscar un texto por su ID
    FUNCTION buscar_texto_por_id(id) RESULT(encontrado)
        CHARACTER(LEN=*), INTENT(IN) :: id
        logical :: encontrado
        integer :: i

        encontrado = .FALSE.

        if (.NOT. ALLOCATED(texto_array)) then
            print *, "No hay textos"
        else
            DO i = 1, size(texto_array)
                if (trim(texto_array(i)%id) == trim(id)) then
                    encontrado = .TRUE.
                    return
                end if
            END DO
        end if
    END FUNCTION buscar_texto_por_id

END MODULE texto



MODULE areaTexto
implicit none

    type :: TextArea
        CHARACTER(LEN = 50) :: id
        CHARACTER(LEN = 20) :: alto
        CHARACTER(LEN = 20) :: ancho
        CHARACTER(LEN = 200) :: texto
        CHARACTER(LEN = 50) :: posicion_x
        CHARACTER(LEN = 50) :: posicion_y
    End type TextArea

    ! Declaración de un arreglo de Tag para almacenar los etiquetas
    type(TextArea), ALLOCATABLE ::  areaTexto_array(:)

contains

    subroutine agregar_areaTexto(id)
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(TextArea) :: nuevo_areaTexto
        integer :: n
        type(TextArea), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo areaTexto
        nuevo_areaTexto%id = id
        nuevo_areaTexto%alto = "150"
        nuevo_areaTexto%ancho = "150"
        nuevo_areaTexto%texto = ""
        nuevo_areaTexto%posicion_x = ""
        nuevo_areaTexto%posicion_y = ""

        ! Agrego el nuevo areaTexto a la lista de areaTextos
        if (.NOT. ALLOCATED(areaTexto_array)) then !Si esta vacia
            ALLOCATE(areaTexto_array(1)) ! Se le asigna memoria para un areaTexto de la lista
            areaTexto_array(1) =  nuevo_areaTexto !Se convierte en el areaTexto nuevo
        else
            n = size(areaTexto_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = areaTexto_array !Reservo memoria
            temp_array(n+1) = nuevo_areaTexto
            DEALLOCATE(areaTexto_array) !Libero memoria
            ALLOCATE(areaTexto_array(n+1)) !Reservo memoria de nuevo
            areaTexto_array = temp_array
        end if
    end subroutine agregar_areaTexto

    subroutine imprimir_areaTextos()
        
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(areaTexto_array)) then
            print *, "No hay areaTextos"
        else
            print *, "areaTextos encontrados: ", size(areaTexto_array)
            DO i = 1, size(areaTexto_array)
                print *, 'id: ', trim(areaTexto_array(i)%id)
                print *, 'alto: ', trim(areaTexto_array(i)%alto)
                print *, 'ancho: ', trim(areaTexto_array(i)%ancho)
                print *, 'texto: ', trim(areaTexto_array(i)%texto)
                print *, 'posicion_x: ', trim(areaTexto_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(areaTexto_array(i)%posicion_y)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_areaTextos

    subroutine areaTexto_set_texto(id, texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(areaTexto_array)) then
            print *, "No hay areaTextos"
        else
            DO i = 1, size(areaTexto_array)
                if (trim(areaTexto_array(i)%id) == id) then
                    areaTexto_array(i)%texto = texto
                end if
            END DO
        end if

    end subroutine areaTexto_set_texto

    subroutine areaTexto_set_posicion(id, posicion_x, posicion_y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: posicion_x
        CHARACTER(LEN=*), INTENT(IN) :: posicion_y
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(areaTexto_array)) then
            print *, "No hay areaTextos"
        else
            DO i = 1, size(areaTexto_array)
                if (trim(areaTexto_array(i)%id) == id) then
                    areaTexto_array(i)%posicion_x = posicion_x
                    areaTexto_array(i)%posicion_y = posicion_y
                end if
            END DO
        end if

    end subroutine areaTexto_set_posicion

    ! Función para buscar un areaTexto por su ID
    FUNCTION buscar_areaTexto_por_id(id) RESULT(encontrado)
        CHARACTER(LEN=*), INTENT(IN) :: id
        logical :: encontrado
        integer :: i

        encontrado = .FALSE.

        if (.NOT. ALLOCATED(areaTexto_array)) then
            print *, "No hay areaTextos"
        else
            DO i = 1, size(areaTexto_array)
                if (trim(areaTexto_array(i)%id) == trim(id)) then
                    encontrado = .TRUE.
                    return
                end if
            END DO
        end if
    END FUNCTION buscar_areaTexto_por_id

END MODULE areaTexto



MODULE clave
implicit none

    type :: Password
        CHARACTER(LEN = 50) :: id
        CHARACTER(LEN = 20) :: alto
        CHARACTER(LEN = 20) :: ancho
        CHARACTER(LEN = 200) :: texto
        CHARACTER(LEN =20) :: alineacion
        CHARACTER(LEN = 50) :: posicion_x
        CHARACTER(LEN = 50) :: posicion_y
    End type Password

    ! Declaración de un arreglo de Tag para almacenar los etiquetas
    type(Password), ALLOCATABLE ::  clave_array(:)

contains

    subroutine agregar_clave(id)
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(Password) :: nueva_clave
        integer :: n
        type(Password), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos de la nueva clave
        nueva_clave%id = id
        nueva_clave%alto = "25"
        nueva_clave%ancho = "100"
        nueva_clave%texto = ""
        nueva_clave%alineacion = "izquierda"
        nueva_clave%posicion_x = ""
        nueva_clave%posicion_y = ""

        ! Agrego la nueva clave a la lista de claves
        if (.NOT. ALLOCATED(clave_array)) then !Si esta vacia
            ALLOCATE(clave_array(1)) ! Se le asigna memoria para una clave de la lista
            clave_array(1) =  nueva_clave !Se convierte en la clave nueva
        else
            n = size(clave_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = clave_array !Reservo memoria
            temp_array(n+1) = nueva_clave
            DEALLOCATE(clave_array) !Libero memoria
            ALLOCATE(clave_array(n+1)) !Reservo memoria de nuevo
            clave_array = temp_array
        end if
    end subroutine agregar_clave

    subroutine imprimir_claves()
        
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(clave_array)) then
            print *, "No hay claves"
        else
            print *, "claves encontradas: ", size(clave_array)
            DO i = 1, size(clave_array)
                print *, 'id: ', trim(clave_array(i)%id)
                print *, 'alto: ', trim(clave_array(i)%alto)
                print *, 'ancho: ', trim(clave_array(i)%ancho)
                print *, 'texto: ', trim(clave_array(i)%texto)
                print *, 'alineacion: ', trim(clave_array(i)%alineacion)
                print *, 'posicion_x: ', trim(clave_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(clave_array(i)%posicion_y)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_claves

    subroutine clave_set_texto(id, texto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: texto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(clave_array)) then
            print *, "No hay claves"
        else
            DO i = 1, size(clave_array)
                if (trim(clave_array(i)%id) == id) then
                    clave_array(i)%texto = texto
                end if
            END DO
        end if

    end subroutine clave_set_texto

    subroutine clave_set_alineacion(id, alineacion)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: alineacion
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(clave_array)) then
            print *, "No hay claves"
        else
            DO i = 1, size(clave_array)
                if (trim(clave_array(i)%id) == id) then
                    clave_array(i)%alineacion = alineacion
                end if
            END DO
        end if

    end subroutine clave_set_alineacion

    subroutine clave_set_posicion(id, posicion_x, posicion_y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: posicion_x
        CHARACTER(LEN=*), INTENT(IN) :: posicion_y
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(clave_array)) then
            print *, "No hay claves"
        else
            DO i = 1, size(clave_array)
                if (trim(clave_array(i)%id) == id) then
                    clave_array(i)%posicion_x = posicion_x
                    clave_array(i)%posicion_y = posicion_y
                end if
            END DO
        end if

    end subroutine clave_set_posicion

    ! Función para buscar una clave por su ID
    FUNCTION buscar_clave_por_id(id) RESULT(encontrado)
        CHARACTER(LEN=*), INTENT(IN) :: id
        logical :: encontrado
        integer :: i

        encontrado = .FALSE.

        if (.NOT. ALLOCATED(clave_array)) then
            print *, "No hay claves"
        else
            DO i = 1, size(clave_array)
                if (trim(clave_array(i)%id) == trim(id)) then
                    encontrado = .TRUE.
                    return
                end if
            END DO
        end if
    END FUNCTION buscar_clave_por_id

END MODULE clave



MODULE contenedor
implicit none

    type :: Container
        CHARACTER(LEN = 50) :: id
        CHARACTER(LEN = 20) :: alto
        CHARACTER(LEN = 20) :: ancho
        CHARACTER(LEN = 50) :: color_fondo_r
        CHARACTER(LEN = 50) :: color_fondo_g
        CHARACTER(LEN = 50) :: color_fondo_b
        CHARACTER(LEN = 50) :: posicion_x
        CHARACTER(LEN = 50) :: posicion_y
    End type Container

    ! Declaración de un arreglo de Tag para almacenar los etiquetas
    type(Container), ALLOCATABLE ::  contenedor_array(:)

contains

    subroutine agregar_contenedor(id)
        CHARACTER(LEN=*), INTENT(IN) :: id

        type(Container) :: nuevo_contenedor
        integer :: n
        type(Container), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo contenedor
        nuevo_contenedor%id = id
        nuevo_contenedor%alto = ""
        nuevo_contenedor%ancho = ""
        nuevo_contenedor%color_fondo_r = ""
        nuevo_contenedor%color_fondo_g = ""
        nuevo_contenedor%color_fondo_b = ""
        nuevo_contenedor%posicion_x = ""
        nuevo_contenedor%posicion_y = ""

        ! Agrego el nuevo contenedor a la lista de contenedores
        if (.NOT. ALLOCATED(contenedor_array)) then !Si esta vacia
            ALLOCATE(contenedor_array(1)) ! Se le asigna memoria para un contenedor de la lista
            contenedor_array(1) =  nuevo_contenedor !Se convierte en el contenedor nuevo
        else
            n = size(contenedor_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = contenedor_array !Reservo memoria
            temp_array(n+1) = nuevo_contenedor
            DEALLOCATE(contenedor_array) !Libero memoria
            ALLOCATE(contenedor_array(n+1)) !Reservo memoria de nuevo
            contenedor_array = temp_array
        end if
    end subroutine agregar_contenedor

    subroutine imprimir_contenedores()
        
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(contenedor_array)) then
            print *, "No hay contenedores"
        else
            print *, "contenedores encontrados: ", size(contenedor_array)
            DO i = 1, size(contenedor_array)
                print *, 'id: ', trim(contenedor_array(i)%id)
                print *, 'alto: ', trim(contenedor_array(i)%alto)
                print *, 'ancho: ', trim(contenedor_array(i)%ancho)
                print *, 'color_fondo_r: ', trim(contenedor_array(i)%color_fondo_r)
                print *, 'color_fondo_g: ', trim(contenedor_array(i)%color_fondo_g)
                print *, 'color_fondo_b: ', trim(contenedor_array(i)%color_fondo_b)
                print *, 'posicion_x: ', trim(contenedor_array(i)%posicion_x)
                print *, 'posicion_y: ', trim(contenedor_array(i)%posicion_y)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_contenedores

    subroutine contenedor_set_alto(id, alto)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: alto
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(contenedor_array)) then
            print *, "No hay contenedores"
        else
            DO i = 1, size(contenedor_array)
                if (trim(contenedor_array(i)%id) == id) then
                    contenedor_array(i)%alto = alto
                end if
            END DO
        end if

    end subroutine contenedor_set_alto

    subroutine contenedor_set_ancho(id, ancho)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: ancho
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(contenedor_array)) then
            print *, "No hay contenedores"
        else
            DO i = 1, size(contenedor_array)
                if (trim(contenedor_array(i)%id) == id) then
                    contenedor_array(i)%ancho = ancho
                end if
            END DO
        end if

    end subroutine contenedor_set_ancho

    subroutine contenedor_set_color_fondo(id, color_fondo_r, color_fondo_g, color_fondo_b)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: color_fondo_r
        CHARACTER(LEN=*), INTENT(IN) :: color_fondo_g
        CHARACTER(LEN=*), INTENT(IN) :: color_fondo_b
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(contenedor_array)) then
            print *, "No hay contenedores"
        else
            DO i = 1, size(contenedor_array)
                if (trim(contenedor_array(i)%id) == id) then
                    contenedor_array(i)%color_fondo_r = color_fondo_r
                    contenedor_array(i)%color_fondo_g = color_fondo_g
                    contenedor_array(i)%color_fondo_b = color_fondo_b
                end if
            END DO
        end if

    end subroutine contenedor_set_color_fondo

    subroutine contenedor_set_posicion(id, posicion_x, posicion_y)
        CHARACTER(LEN=*), INTENT(IN) :: id
        CHARACTER(LEN=*), INTENT(IN) :: posicion_x
        CHARACTER(LEN=*), INTENT(IN) :: posicion_y
        integer :: i

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(contenedor_array)) then
            print *, "No hay contenedores"
        else
            DO i = 1, size(contenedor_array)
                if (trim(contenedor_array(i)%id) == id) then
                    contenedor_array(i)%posicion_x = posicion_x
                    contenedor_array(i)%posicion_y = posicion_y
                end if
            END DO
        end if

    end subroutine contenedor_set_posicion

    ! Función para buscar un contenedor por su ID
    FUNCTION buscar_contenedor_por_id(id) RESULT(encontrado)
        CHARACTER(LEN=*), INTENT(IN) :: id
        logical :: encontrado
        integer :: i

        encontrado = .FALSE.

        if (.NOT. ALLOCATED(contenedor_array)) then
            print *, "No hay contenedores"
        else
            DO i = 1, size(contenedor_array)
                if (trim(contenedor_array(i)%id) == trim(id)) then
                    encontrado = .TRUE.
                    return
                end if
            END DO
        end if
    END FUNCTION buscar_contenedor_por_id

END MODULE contenedor

MODULE token
    use error
    use etiqueta
    use boton
    use check
    use radioBoton
    use texto
    use areaTexto
    use clave
    use contenedor

    implicit none

    type :: Tkn
        CHARACTER(LEN = 100) :: lexema
        CHARACTER(LEN = 200) :: tipo 
        integer :: fila
        integer :: columna
    End type Tkn

    ! Declaración de un arreglo de Tkn para almacenar los tokens
    type(Tkn), ALLOCATABLE ::  token_array(:)
    

contains

    ! Subrutina para agregar tokens a la lista de token
    subroutine agregar_token(lexema, tipo, fila, columna)
        CHARACTER(LEN=*), INTENT(IN) :: lexema
        CHARACTER(LEN=*), INTENT(IN) :: tipo
        integer :: fila
        integer :: columna
        type(Tkn) :: nuevo_token
        integer :: n
        type(Tkn), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo token
        nuevo_token%lexema = lexema
        nuevo_token%tipo = tipo
        nuevo_token%fila = fila
        nuevo_token%columna = columna

        ! Agrego el nuevo token a la lista de tokens
        if (.NOT. ALLOCATED(token_array)) then !Si esta vacia
            ALLOCATE(token_array(1)) ! Se le asigna memoria para un token de la lista
            token_array(1) =  nuevo_token !Se convierte en el token nuevo
        else
            n = size(token_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = token_array !Reservo memoria
            temp_array(n+1) = nuevo_token
            DEALLOCATE(token_array) !Libero memoria
            ALLOCATE(token_array(n+1)) !Reservo memoria de nuevo
            token_array = temp_array
        end if
    end subroutine agregar_token

    ! Subrutina para imprimir el listado de tokens en html

    subroutine generar_html_tokens()
        integer :: i
        character(len=20) :: str_fila, str_columna, str_i
        character(len=100) :: str_lexema, char_token
        integer :: file_unit, ios
        character(len=*), parameter :: RUTA_ARCHIVO = 'Archivos/tokens.html'  ! Nueva constante para la ruta
        
        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(token_array)) then
            print *, "No hay tokens"
        else
            ! Abrir el archivo para escribir
            open(unit=file_unit, file=RUTA_ARCHIVO, status="replace", action="write", iostat=ios)
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
                do i = 1, size(token_array)
                    write(str_i, '(I0)') i
                    write(str_lexema, '(A)') trim(token_array(i)%lexema)
                    write(str_columna, '(I0)') token_array(i)%columna
                    write(str_fila, '(I0)')  token_array(i)%fila
                    write(char_token, '(A)') trim(token_array(i)%tipo)
        
                    ! Escribir cada fila directamente al archivo
                    write(file_unit, '(A)') '<tr><td>' // trim(str_i) // '</td><td>' // char_token // '</td><td>' // trim(str_lexema) // &
                    '</td><td>' // trim(str_fila) // '</td><td>'&
                        // trim(str_columna) // '</td></tr>' // new_line('a')
                end do
                ! Cerrar la tabla y el HTML
                write(file_unit, '(A)') '</table></body></html>'
                close(file_unit)
            end if
        end if
    
    end subroutine generar_html_tokens

    subroutine imprimir_tokens()
        integer :: i
        character(len=20) :: str_fila, str_columna

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(token_array)) then
            print *, "No hay tokens"
        else
            print *, "tokens encontrados: ", size(token_array)
            ! Recorre la lista de tokens y genera una fila para cada token
            DO i = 1, size(token_array)

                write(str_fila, '(I0)') token_array(i)%fila
                write(str_columna, '(I0)') token_array(i)%columna
            
                print *, 'lexema: ', trim(token_array(i)%lexema)
                print *, 'tipo: ', trim(token_array(i)%tipo)
                print *, 'fila: ', trim(str_fila)
                print *, 'columna: ', trim(str_columna)
                print *, '---------------------------------'
            END DO
        end if

    end subroutine imprimir_tokens

    subroutine parser()

        integer :: i, j
        logical :: is_controles, is_propiedades, is_colocacion

        is_controles = .false.
        is_propiedades = .false.
        is_colocacion = .false.

        print *, "Iniciando analisis sintactico"

        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(token_array)) then
            print *, "No hay tokens"
        else

            do i = 1, size(token_array)
                if (token_array(i)%tipo == 'tk_etiqueta') then
                    if (token_array(i+1)%tipo == 'tk_id' .and. token_array(i+2)%tipo == 'tk_puntoComa') then
                        call agregar_etiqueta(token_array(i+1)%lexema)
                    end if
                elseif (token_array(i)%tipo == 'tk_boton') then
                    if (token_array(i+1)%tipo == 'tk_id' .and. token_array(i+2)%tipo == 'tk_puntoComa') then
                        call agregar_boton(token_array(i+1)%lexema)
                    end if
                elseif (token_array(i)%tipo == 'tk_check') then
                    if (token_array(i+1)%tipo == 'tk_id' .and. token_array(i+2)%tipo == 'tk_puntoComa') then
                        call agregar_check(token_array(i+1)%lexema)
                    end if
                elseif (token_array(i)%tipo == 'tk_radioBoton') then
                    if (token_array(i+1)%tipo == 'tk_id' .and. token_array(i+2)%tipo == 'tk_puntoComa') then
                        call agregar_radioBoton(token_array(i+1)%lexema)
                    end if
                elseif (token_array(i)%tipo == 'tk_texto') then
                    if (token_array(i+1)%tipo == 'tk_id' .and. token_array(i+2)%tipo == 'tk_puntoComa') then
                        call agregar_texto(token_array(i+1)%lexema)
                    end if
                elseif (token_array(i)%tipo == 'tk_areaTexto') then
                    if (token_array(i+1)%tipo == 'tk_id' .and. token_array(i+2)%tipo == 'tk_puntoComa') then
                        call agregar_areaTexto(token_array(i+1)%lexema)
                    end if
                elseif (token_array(i)%tipo == 'tk_clave') then
                    if (token_array(i+1)%tipo == 'tk_id' .and. token_array(i+2)%tipo == 'tk_puntoComa') then
                        call agregar_clave(token_array(i+1)%lexema)
                    end if
                elseif (token_array(i)%tipo == 'tk_contenedor') then
                    if (token_array(i+1)%tipo == 'tk_id' .and. token_array(i+2)%tipo == 'tk_puntoComa') then
                        call agregar_contenedor(token_array(i+1)%lexema)
                    end if
                end if

                if (token_array(i)%tipo == 'tk_id' .and. token_array(i+1)%tipo == 'tk_punto' ) then

                    if(token_array(i+2)%tipo == 'tk_setAncho') then
                        
                        do j=1, size(etiqueta_array)
                            
                            if (trim(etiqueta_array(j)%id) == token_array(i)%lexema) then
                                
                                call etiqueta_set_ancho(token_array(i)%lexema,token_array(i+4)%lexema)
                            end if
                        end do
                        do j=1, size(contenedor_array)
                            
                            if (trim(contenedor_array(j)%id) == token_array(i)%lexema) then
                                
                                call contenedor_set_ancho(token_array(i)%lexema,token_array(i+4)%lexema)
                            end if
                        end do
                        
                        


                    end if

                    if(token_array(i+2)%tipo == 'tk_setAlto') then
                        
                        do j=1, size(etiqueta_array)
                            if (trim(etiqueta_array(j)%id) == token_array(i)%lexema) then
                                call etiqueta_set_alto(token_array(i)%lexema,token_array(i+4)%lexema)
                            end if
                        end do
                        do j=1, size(contenedor_array)
                            if (trim(contenedor_array(j)%id) == token_array(i)%lexema) then
                                call contenedor_set_alto(token_array(i)%lexema,token_array(i+4)%lexema)
                            end if
                        end do
                            
                        

                    end if


                    if(token_array(i+2)%tipo == 'tk_setTexto') then
                        
                        do j=1, size(etiqueta_array)
                            if (trim(etiqueta_array(j)%id) == token_array(i)%lexema) then
                                call etiqueta_set_texto(token_array(i)%lexema,token_array(i+5)%lexema)
                            end if
                        end do
                        do j=1, size(boton_array)
                            if (trim(boton_array(j)%id) == token_array(i)%lexema) then
                                call boton_set_texto(token_array(i)%lexema,token_array(i+5)%lexema)
                            end if
                        end do
                        do j=1, size(check_array)
                            if (trim(check_array(j)%id) == token_array(i)%lexema) then
                                call check_set_texto(token_array(i)%lexema,token_array(i+5)%lexema)
                            end if
                        end do
                        do j=1, size(radioBoton_array)
                            if (trim(radioBoton_array(j)%id) == token_array(i)%lexema) then
                                call radioBoton_set_texto(token_array(i)%lexema,token_array(i+5)%lexema)
                            end if
                        end do
                        do j=1, size(texto_array)
                            if (trim(texto_array(j)%id) == token_array(i)%lexema) then
                                call texto_set_texto(token_array(i)%lexema,token_array(i+5)%lexema)
                            end if
                        end do
                        do j=1, size(areaTexto_array)
                            if (trim(areaTexto_array(j)%id) == token_array(i)%lexema) then
                                call areaTexto_set_texto(token_array(i)%lexema,token_array(i+5)%lexema)
                            end if
                        end do
                        do j=1, size(clave_array)
                            if (trim(clave_array(j)%id) == token_array(i)%lexema) then
                                call clave_set_texto(token_array(i)%lexema,token_array(i+5)%lexema)
                            end if
                        end do

                    end if

                    if(token_array(i+2)%tipo == 'tk_setColorLetra') then
                        
                        do j=1, size(etiqueta_array)
                            if (trim(etiqueta_array(j)%id) == token_array(i)%lexema) then
                                call etiqueta_set_color_texto(token_array(i)%lexema,token_array(i+4)%lexema, token_array(i+6)%lexema, token_array(i+8)%lexema)
                            end if
                        end do

                    end if

                    if(token_array(i+2)%tipo == 'tk_setPosicion') then
                    
                        do j=1, size(etiqueta_array)
                            if (trim(etiqueta_array(j)%id) == token_array(i)%lexema) then
                                call etiqueta_set_posicion(token_array(i)%lexema,token_array(i+4)%lexema,token_array(i+6)%lexema)
                            end if
                        end do
                        do j=1, size(boton_array)
                            if (trim(boton_array(j)%id) == token_array(i)%lexema) then
                                call boton_set_posicion(token_array(i)%lexema,token_array(i+4)%lexema,token_array(i+6)%lexema)
                            end if
                        end do
                        do j=1, size(check_array)
                            if (trim(check_array(j)%id) == token_array(i)%lexema) then
                                call check_set_posicion(token_array(i)%lexema,token_array(i+4)%lexema,token_array(i+6)%lexema)
                            end if
                        end do
                        do j=1, size(radioBoton_array)
                            if (trim(radioBoton_array(j)%id) == token_array(i)%lexema) then
                                call radioBoton_set_posicion(token_array(i)%lexema,token_array(i+4)%lexema,token_array(i+6)%lexema)
                            end if
                        end do
                        do j=1, size(texto_array)
                            if (trim(texto_array(j)%id) == token_array(i)%lexema) then
                                call texto_set_posicion(token_array(i)%lexema,token_array(i+4)%lexema,token_array(i+6)%lexema)
                            end if
                        end do
                        do j=1, size(areaTexto_array)
                            if (trim(areaTexto_array(j)%id) == token_array(i)%lexema) then
                                call areaTexto_set_posicion(token_array(i)%lexema,token_array(i+4)%lexema,token_array(i+6)%lexema)
                            end if
                        end do
                        do j=1, size(clave_array)
                            if (trim(clave_array(j)%id) == token_array(i)%lexema) then
                                call clave_set_posicion(token_array(i)%lexema,token_array(i+4)%lexema,token_array(i+6)%lexema)
                            end if
                        end do
                        do j=1, size(contenedor_array)
                            if (trim(contenedor_array(j)%id) == token_array(i)%lexema) then
                                call contenedor_set_posicion(token_array(i)%lexema,token_array(i+4)%lexema,token_array(i+6)%lexema)
                            end if
                        end do


                    end if

                    if(token_array(i+2)%tipo == 'tk_setAlineacion') then
                        do j=1, size(boton_array)
                            if (trim(boton_array(j)%id) == token_array(i)%lexema) then
                                call boton_set_alineacion(token_array(i)%lexema,token_array(i+4)%lexema)
                            end if
                        end do
                        do j=1, size(texto_array)
                            if (trim(texto_array(j)%id) == token_array(i)%lexema) then
                                call texto_set_alineacion(token_array(i)%lexema,token_array(i+4)%lexema)
                            end if
                        end do
                        do j=1, size(clave_array)
                            if (trim(clave_array(j)%id) == token_array(i)%lexema) then
                                call clave_set_alineacion(token_array(i)%lexema,token_array(i+4)%lexema)
                            end if
                        end do
                    end if

                    if (token_array(i+2)%tipo == 'tk_setColorFondo') then
                        
                        do j=1, size(etiqueta_array)
                            if (trim(etiqueta_array(j)%id) == token_array(i)%lexema) then
                                call etiqueta_set_color_fondo(token_array(i)%lexema,token_array(i+4)%lexema, token_array(i+6)%lexema, token_array(i+8)%lexema)
                            end if
                        end do
                        do j=1, size(contenedor_array)
                            if (trim(contenedor_array(j)%id) == token_array(i)%lexema) then
                                call contenedor_set_color_fondo(token_array(i)%lexema,token_array(i+4)%lexema, token_array(i+6)%lexema, token_array(i+8)%lexema)
                            end if
                        end do
                    end if

                    if (token_array(i+2)%tipo == 'tk_setMarcada') then
                        
                        do j=1, size(check_array)
                            if (trim(check_array(j)%id) == token_array(i)%lexema) then
                                call check_set_marcado(token_array(i)%lexema, token_array(i+4)%lexema)
                            end if
                        end do
                        do j=1, size(radioBoton_array)
                            if (trim(radioBoton_array(j)%id) == token_array(i)%lexema) then
                                call radioBoton_set_marcado(token_array(i)%lexema, token_array(i+4)%lexema)
                            end if
                        end do
                    end if
                    if (token_array(i+2)%tipo == 'tk_setGrupo') then
                        do j=1, size(radioBoton_array)
                            if (trim(radioBoton_array(j)%id) == token_array(i)%lexema) then
                                call radioBoton_set_grupo(token_array(i)%lexema, token_array(i+4)%lexema)
                            end if
                        end do
                    end if

                end if
            
            end do
        end if



    end subroutine parser
    

END MODULE token



program analizador_lexico
    use error
    use token

    implicit none
    integer :: len, fila, columna, estado, puntero
    integer :: ios, unidad
    character(len=100000) :: contenido, buffer
    character(len=1) :: char
    character(len=100) :: aux_tkn
    logical :: comentario_linea
    integer :: numErrores

    estado = 0
    puntero = 1
    columna = 0
    fila = 1
    aux_tkn = ""
    numErrores = 0

    contenido = ""

    comentario_linea = .false.

    unidad = 10

    
    do
        read(*, '(A)', IOSTAT=ios) buffer
        if (ios /= 0) exit
        contenido = trim(contenido) // trim(buffer) // new_line('a') ! concatenamos el
        !contenido mas lo que viene en el buffer y como leemos por el salto de linea al final
    end do

    len = len_trim(contenido)

    do while(puntero <= len)
        char = contenido(puntero:puntero)
        select case (estado)
            case (0)
                ! Verifica que el carácter sea un simbolo
                if (char == ';' .or. char == '-' .or. char == '<' .or. char == '>' .or. char == '!' .or. char == ',' .or. char == '.' .or. char == '(' .or. char == ')' .or. char == '"') then
                    estado = 1
                    columna = columna + 1
                elseif ( char >= 'A' .and. char <= 'Z' .or. (char >= 'a' .and. char <= 'z') ) then
                    estado = 2
                
                elseif (char >= '0' .and. char <= '9' ) then
                    estado = 3

                elseif (char == '/') then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1 
                    estado = 4

                elseif (ichar(char) == 10) then ! Actualizo la posicion
                    ! Salto de línea
                    comentario_linea = .false.
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
                    CALL agregar_error("Lexico", fila, columna, char, 'Caracter: ' // char // ' no valido')
                    columna = columna + 1
                    puntero = puntero + 1
                    numErrores = numErrores + 1 

                end if
                
            case (1)
                if ( char == ';' ) then
                    call agregar_token(char, 'tk_puntoComa', fila, columna)

                elseif ( char == '>') then
                call agregar_token(char, 'tk_mayorQue', fila, columna)

                elseif ( char == '<') then
                    call agregar_token(char, 'tk_menorQue', fila, columna)

                elseif ( char == '.') then
                    call agregar_token(char, 'tk_punto', fila, columna)

                elseif ( char == ',') then
                    call agregar_token(char, 'tk_coma', fila, columna)

                elseif ( char == '(') then
                    call agregar_token(char, 'tk_parentesisAbre', fila, columna)

                elseif ( char == ')') then
                    call agregar_token(char, 'tk_parentesisCierra', fila, columna)
                
                elseif ( char == '-') then
                    call agregar_token(char, 'tk_guion', fila, columna)
                
                elseif ( char == '!') then
                    call agregar_token(char, 'tk_exclamacion', fila, columna) 

                elseif ( char == '"') then
                    call agregar_token(char, 'tk_comillas', fila, columna)

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

                    elseif ((aux_tkn == 'Check')) then
                        call agregar_token(aux_tkn, 'tk_check', fila, columna)

                    elseif ((aux_tkn == 'RadioBoton')) then
                        call agregar_token(aux_tkn, 'tk_radioBoton', fila, columna)

                    elseif ((aux_tkn == 'Texto')) then
                        call agregar_token(aux_tkn, 'tk_texto', fila, columna)

                    elseif ((aux_tkn == 'AreaTexto')) then
                        call agregar_token(aux_tkn, 'tk_areaTexto', fila, columna)

                    elseif ((aux_tkn == 'Clave')) then
                        call agregar_token(aux_tkn, 'tk_clave', fila, columna)
                    
                    elseif ((aux_tkn == 'Boton')) then
                        call agregar_token(aux_tkn, 'tk_boton', fila, columna)

                    elseif ((aux_tkn == 'controles' .or. aux_tkn == 'CONTROLES' .or. aux_tkn == 'Controles')) then
                        call agregar_token(aux_tkn, 'tk_controles', fila, columna)

                    elseif ((aux_tkn == 'propiedades' .or. aux_tkn == 'PROPIEDADES' .or. aux_tkn == 'Propiedades')) then
                        call agregar_token(aux_tkn, 'tk_propiedades', fila, columna)

                    elseif ((aux_tkn == 'colocacion' .or. aux_tkn == 'COLOCACION' .or. aux_tkn == 'Colocacion')) then
                        call agregar_token(aux_tkn, 'tk_colocacion', fila, columna)
                    
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

                    elseif ((aux_tkn == 'setAlineacion')) then
                        call agregar_token(aux_tkn, 'tk_setAlineacion', fila, columna)

                    elseif ((aux_tkn == 'setPosicion')) then
                        call agregar_token(aux_tkn, 'tk_setPosicion', fila, columna)

                    elseif ((aux_tkn == 'setMarcada')) then
                        call agregar_token(aux_tkn, 'tk_setMarcada', fila, columna)

                    elseif ((aux_tkn == 'true')) then
                        call agregar_token(aux_tkn, 'tk_true', fila, columna)

                    elseif ((aux_tkn == 'false')) then
                        call agregar_token(aux_tkn, 'tk_false', fila, columna)

                    elseif ((aux_tkn == 'setGrupo')) then
                        call agregar_token(aux_tkn, 'tk_setGrupo', fila, columna)

                    elseif ((aux_tkn == 'setAncho')) then
                        call agregar_token(aux_tkn, 'tk_setAncho', fila, columna)

                    elseif ((aux_tkn == 'setAlto')) then
                        call agregar_token(aux_tkn, 'tk_setAlto', fila, columna)
                    
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

                if (char == "/") then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1 
                    comentario_linea = .true.
                    estado = 5
                
                elseif (char == '*') then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1 

                    estado = 6
                
                else
                    ! call agregar_error(aux_tkn, 'Error lexico', fila, columna)

                    aux_tkn = ""
                    estado = 0
                end if

            case (5)
                if (ichar(char) == 10) then
                    ! Salto de línea
                    comentario_linea = .false.
                    columna = 0
                    fila = fila + 1
                    puntero = puntero + 1
                    call agregar_token(aux_tkn, 'tk_comentario', fila, columna)
                    aux_tkn = ""
                    estado = 0
                elseif (comentario_linea) then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1
                end if
                
            case (6)
                if (char /= "*") then
                    aux_tkn = trim(aux_tkn) // char 
                    if (ichar(char) == 10) then 
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
                        columna = columna + 1
                        puntero = puntero + 1
                    end if
                else
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1
                    estado = 7
                end if             
            case (7)
                if (char == "/") then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1 
                    estado = 0
                    aux_tkn = ""
                else
                    aux_tkn = trim(aux_tkn) // char
                    if (ichar(char) == 10) then 
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
                        columna = columna + 1
                        puntero = puntero + 1
                    end if
                    estado = 6
                end if
        end select
    end do
    
    call parser

    if (numErrores > 0) then
        call generar_json_errores
    else
        call generar_html_tokens
    end if

    call imprimir_etiquetas
    call imprimir_botones
    call imprimir_checks
    call imprimir_radioBotones
    call imprimir_textos
    call imprimir_areaTextos
    call imprimir_claves
    call imprimir_contenedores
        
end program analizador_lexico