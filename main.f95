program main
 
    use matrix_m
    use abb_m
    use AVL_Tree_M
    use List_of_lists_m
    implicit none
    
    type(abb) :: treeABB
    type(matrix_t) :: matrixD
    type(Tree_t) :: treeAVL
    type(List_of_lists) :: listAlbums
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
                print *, "Opcion Invalida."
        end select
    end do
    
contains
    
    subroutine register()
        ! Implement user registration logic here
        print *, "Registrar usuario ----"
        ! TODO: al ya tener el arbol, crear el metodo para añadir un usuario
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
            call user_menu(username) ! Pasar el nombre para tener esos datos de sesion en el menu
            print *, "Usuario logueado"
        else
            print *, "Nombre de usuario o contraseña incorrectos."
             
        end if
    end subroutine
    
    subroutine user_menu(username)
        character(len=20), intent(in) :: username
        integer :: choiceU = 0
        do while (choiceU /= 4)
            print *, "Bienvenido, ", username
            print *, "1. Visualizar reportes de las estructuras"
            print *, "2. Navegacion y gestion de imagenes"
            print *, "3. Opciones de carga masiva"
            print *, "4. Salir"
            print *, "Seleccione una opcion:"
            read *, choiceU
            select case (choiceU)
                case (1)
                    !call reports_menu(username)
                case (2)
                    call image_menu(username)
                case (3)
                    call mass_upload_menu(username)
                case (4)
                    exit
                case default
                    print *, "Opcion Invalida."
            end select
        end do
    end subroutine

    subroutine mass_upload_menu(username)
        character(len=20), intent(in) :: username
        integer :: choiceUM = 0
        character(:), allocatable :: filename
        do while (choiceUM /= 4)
            print *, "----------", trim(username), "----------"
            print *, "1. Cargar Capas"
            print *, "2. Cargar Imagenes"
            print *, "3. Cargar Albumes"
            print *, "4. Salir"
            print *, "Seleccione una opcion:"
            read *, choiceUM
            select case (choiceUM)
                case (1)
                    call cargar_capas(username) 
                case (2)
                    call cargar_imagenes(username)
                case (3)
                    call cargar_albumes(username)
                case (4)
                    exit
                case default
                    print *, "Opcion Invalida."
            end select
        end do
    end subroutine

    subroutine cargar_imagenes(username)
        use json_module
        implicit none
    
        type(pixel), dimension(:), allocatable :: pixelsI
        type(pixel) :: pixel_tmpI
        type(json_file) :: json
        type(json_value), pointer :: list_p, attribute_p, imagen_p, capas_array_p
        type(json_core) :: jsonc
        integer :: idimg,capas_size, iPixel
        integer :: size,iJ, iCapa,RESULT
        logical :: found
        character(len=10) :: contadorImg
        character(len=20), intent(in) :: username
        character(:), allocatable :: IDIMAGE
        print *, "USUARIO: ", username
        call jsonc%initialize()
        call json%load(filename="D:\EDD_PROYECTO2_202004071\jsonFiles\imagenes.json")
        call json%info('', n_children=size) 
        call json%get_core(jsonc)
        call json%get('', list_p, found)
        call treeAVL%newTree()
        do iJ=1 , size
            call jsonc%get_child(list_p, iJ, imagen_p, found)
            call jsonc%get_child(imagen_p, 'id', attribute_p, found=found)
            if(found) call jsonc%get(attribute_p, idimg)
            print *, "ID Imagen: ", idimg
      
            write(contadorImg, '(I10)') idimg
            
            call matrixD%init()
            call treeAVL%insert(idimg)
            call jsonc%get_parent(capas_array_p,attribute_p)
            call jsonc%get_child(imagen_p, 'capas', capas_array_p, found=found)
            call jsonc%info(capas_array_p, n_children=capas_size)
            do iCapa=1 , capas_size
                call jsonc%get_child(capas_array_p, iCapa, attribute_p, found)
                if(found) call jsonc%get(attribute_p, idimg)
                print *, "ID Capa: ", idimg
                !call treeABB%printPixelsById(idimg)
                !aca con el id de la capa, buscar en el arbol de capas y obtener los pixeles de esa capa para crear la imagen
                pixelsI = treeABB%getPixelsById(idimg)
                 
                do iPixel = 1, treeABB%getPixelSizeById(idimg)
                   ! print *, "Fila: ", pixelsI(iPixel)%Fila, " Columna: ",&
                   !  pixelsI(iPixel)%Columna, " Color: ", pixelsI(iPixel)%Color
                     !error en esto por el allocatable de pixelsI
                     !Hoy ya no da error JAJAJA, ahi se va 

                   !Segun el enunciado aca no se debe de hacer la imgaen sino que aca nada mas se guarda la informacion de la imagen
                    ! y luego en el apartado de creacion de imagen se permite por medio del id de la imagen crear la imagen
                    !entonces en el avl se guarda la imagen, y con sus respectivas capas para luego hacer este getPixelsById otra vez y asi 
                    !ya tener la informacion de la imagen y poder crearla, pero esto mismo se puede hacer con las capas
                    !ya que tendra la opcion de crear imagen, al añadir las capas a usar :O
                     !call matrixD%add(pixelsI(iPixel)%Fila, pixelsI(iPixel)%Columna, pixelsI(iPixel)%Color)
                end do
            end do
            !call matrixD%create_dot(trim(contadorImg))
        end do
        
        call treeAVL%generateGraph()
        call json%destroy()
    end subroutine

    subroutine cargar_albumes(username)
        use json_module
        implicit none
        type(json_file) :: json
        type(json_value), pointer :: list_p, album_p, attribute_p, nombre_p, imags_array_p, imagen_p
        type(json_core) :: jsonc
        integer :: idimg,imags_size
        character(len=20), intent(in) :: username
        character(:), allocatable :: nombre_album
        integer :: size,iJ, iImg
        logical :: found
        call jsonc%initialize()
        call json%load(filename="D:\EDD_PROYECTO2_202004071\jsonFiles\albumes.json")
        call json%info('', n_children=size)
        call json%get_core(jsonc)
        call json%get('', list_p, found)
        do iJ=1 , size
            call jsonc%get_child(list_p, iJ, album_p, found)
            call jsonc%get_child(album_p, 'nombre_album', nombre_p, found=found)
            if(found) call jsonc%get(nombre_p, nombre_album)
            print *, "Nombre Album: ", trim(nombre_album) 
            
            call jsonc%get_child(album_p, 'imgs', imags_array_p, found=found)
            call jsonc%info(imags_array_p, n_children=imags_size)
            do iImg=1 , imags_size
                call jsonc%get_child(imags_array_p, iImg, imagen_p, found)
                if(found) call jsonc%get(imagen_p, idimg)
                print *, "ID Imagen: ", idimg
                call listAlbums%insert(nombre_album, idimg)
            end do
        end do    
        call listAlbums%printList()
        call listAlbums%graphDot("AlbumesGraph.dot")
         
    end subroutine
 
    subroutine cargar_capas(username)! Para cargar capas se procesara un archivo json,                                  
        use json_module
         
        implicit none
        type(json_file) :: json
        type(json_value), pointer :: list_p, capa_p, attribute_p, pixeles_p, pixeles_child_p
        type(json_core) :: jsonc
        !los atributos a leer de este json seran id_capa, pixeles, que sera una lista de
        !fila, columna, color
        character(len=20), intent(in) :: username
        integer :: id_capa, iP, fila, columna
        character(:), allocatable :: color
        integer :: size,iJ,size2,iCC
        logical :: found,insert
        type(pixel), dimension(:), allocatable :: pixels
        type(pixel) :: pixel_tmp
 
        !call matrixD%init()
        call jsonc%initialize()
        call json%load(filename="D:\EDD_PROYECTO2_202004071\jsonFiles\capas.json")
        call json%info('', n_children=size)
        call json%get_core(jsonc)
        call json%get('', list_p, found)  
        do iCC=1 , size
            call jsonc%get_child(list_p, iCC, capa_p, found)
            call jsonc%get_child(capa_p, 'id_capa', attribute_p, found=found)
            if(found) call jsonc%get(attribute_p, id_capa)
            call jsonc%get_child(capa_p, 'pixeles', pixeles_p, found=found)
            call jsonc%info(pixeles_p, n_children=size2)
            print *, "ID Capa: ", id_capa
            print *, "Pixeles: ", size2  
            !añadir el id_capa al arbol binario de busqueda
            allocate(pixels(size2))
            !por cada pixel recorrer sus atributos de fila, columna y color
            do iP = 1, size2
                call jsonc%get_child(pixeles_p, iP, pixeles_child_p, found)
                if (.not. found) cycle  

                call jsonc%get_child(pixeles_child_p, 'fila', attribute_p, found=found)
                if (found) call jsonc%get(attribute_p, fila)

                call jsonc%get_child(pixeles_child_p, 'columna', attribute_p, found=found)
                if (found) call jsonc%get(attribute_p, columna)

                call jsonc%get_child(pixeles_child_p, 'color', attribute_p, found=found)
                if (found) call jsonc%get(attribute_p, color)

                !Para añadir a la matriz tiene que ser row, column, value
                !call matrixD%add(fila, columna, color)
                !Las capas unicamente guardaran en cada nodo con la informacion de 
                !id_capa, guardara los nodos con la informacion del pixel, y ya las imagenes
                !buscaran en esta estructura los id_capa y usaran esa informacion de pixeles
                !para crear la imagen, por eso es que pueden ser varias capas para una imagen
                !las capas las tengo que guardar por el id en el arbol binario de busqueda (ABB)
                pixel_tmp = pixel(fila, columna, color)
                pixels(iP) = pixel_tmp                
                print *, "Fila: ", fila, " Columna: ", columna, " Color: ", color
            end do
            call treeABB%insert(id_capa, pixels)
            deallocate(pixels)
            
        end do
        !call treeABB%printPixels()
        call treeABB%graph("ABBgraph")
        !call matrixD%create_dot()

    end subroutine

    subroutine image_menu(username)
        character(len=20), intent(in) :: username
        print *, "USUARIO: ", username
        ! Implement image management logic here
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
        call json%load(filename="D:\EDD_PROYECTO2_202004071\jsonFiles\usuarios.json")
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