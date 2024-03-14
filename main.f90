program main
    !use btreeds
    implicit none
    
    
    integer :: choice = 0
    do while (choice /= 3)
         
 
        print *, "1. Registro"
        print *, "2. Login"
        print *, "3. Salir"
        print *, "Seleccione una opcion: "

        read *, choice   

        select case (choice)
            case (1)
                call register()
            case (2)
                call login()
            case (3)
                exit
            case default
                print *, "Invalid choice. Please try again."
        end select
    end do
    
contains
    
    subroutine register()
        ! Implement user registration logic here
        print *, "Registrar usuario ----"
        ! Add your code here
    end subroutine
    
    subroutine login()
        character(len=20) :: username
        character(len=20) :: password
        print *, "Datos de login"
        ! ADMIN User: admin, Password: EDD2024
        !Usuarios seran cargados a travez de un archivo json
        print *, "Username: "
        read *, username
        print *, "Password: "
        read *, password
        if (username == "admin" .and. password == "EDD2024") then
            call admin_menu()
        else if (username == "user" .and. password == "user") then
            !call user_menu() 
            print *, "Usuario logueado"
        else
            print *, "Nombre de usuario o contraseña incorrectos."
             
        end if
    end subroutine

    subroutine admin_menu()
        integer :: choiceA = 0
        do while (choiceA /= 3)
            print *, "1. Cargar usuarios"
            print *, "2. Arbol B de usuarios"
            print *, "3. Modificar usuarios"
            print *, "4. Salir"
            print *, "Seleccione una opcion:"
            read *, choiceA
            select case (choiceA)
                case (1)
                    call add_user()
                case (2)
                    
                case (3)
                    !call modify_user()
                case (4)
                    exit
                case default
                    print *, "Opcion Invalida."
            end select
        end do
    end subroutine

    subroutine add_user()
        use json_module
        implicit none
         
        type(json_file) :: json
        type(json_value), pointer :: list_p, client_p, attribute_p
        type(json_core) :: jsonc
        character(:), allocatable :: dpi,nombre_cliente,password
        integer :: size,iJ
        logical :: found
        call jsonc%initialize()
        call json%load(filename="D:\EDD_PROYECTO2_202004071\usuarios.json")
        call json%info('', n_children=size)
        call json%get_core(jsonc)
        call json%get('', list_p, found)
        do iJ=1 , size
    
            call jsonc%get_child(list_p, iJ, client_p, found)
            call jsonc%get_child(client_p, 'nombre_cliente', attribute_p, found=found)
            if(found) call jsonc%get(attribute_p, nombre_cliente)
            call jsonc%get_child(client_p, 'dpi', attribute_p, found=found)
            if(found) call jsonc%get(attribute_p, dpi)
            call jsonc%get_child(client_p, 'password', attribute_p, found=found)
            if(found) call jsonc%get(attribute_p, password)
            print *, "Nombre: ", trim(nombre_cliente), " DPI: ", trim(dpi), " Password: ", trim(password)
            !TODO: Meter este cliente a la lista
           

        end do       
        call json%destroy()  
     
         
    end subroutine
    
end program main