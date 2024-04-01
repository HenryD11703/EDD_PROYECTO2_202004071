program main
 
   use BTree
   use matrix_m
   use abb_m
   use TreeAVL_M
   use List_of_lists_m
   implicit none
   
   type(abb) :: treeABB
   type(matrix_t) :: matrixD
   type(Tree_t) :: treeAVL
   type(List_of_lists) :: listAlbums
   type(BTreeNode), pointer :: rootB => null()
 
   rootB => root


   caLL mainMenu()
   
contains

   subroutine mainMenu()
      integer :: choice = 0
 
   do  

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
         return
      case default
         print *, "Opcion Invalida."
      end select
 
   end do 

   end subroutine

   subroutine register()
      type(UsuarioB) :: usuarioReg
      print *, "Registrar usuario ----"
      print *, "Ingrese el DPI: "
      read *, usuarioReg%id
      print *, "Ingrese el nombre: "
      read *, usuarioReg%nombre
      print *, "Ingrese la contraseña: "
      read *, usuarioReg%contrasena
      call insertB(usuarioReg)
      print *, "Usuario registrado con exito."
      print *, "Actualizando arbol B..."
    
      call writeGraphviz(root,"GraficaArbolB.dot")
      call system("dot -Tpng D:\EDD_PROYECTO2_202004071\executable\GraficaArbolB.dot&
      & -o D:\EDD_PROYECTO2_202004071\executable\GraficaArbolB.png")
      
      return
   end subroutine

   subroutine login()
      use BTree
      character(len=50) :: username = ""
      character(len=50) :: password = ""
      integer(kind=8) :: dpiIngresado
      logical :: isValid
      print *, "Datos de login"
  
      print *, "Username: "
      read *, username
      print *, "Password: "
      read *, password
      if (username == "admin" .and. password == "EDD2024") then
  
          call admin_menu()
  
      else
         read(username,*) dpiIngresado
          isValid = checkUser(dpiIngresado, password)
          if (isValid) then
              call user_menu(username) ! Pasar el nombre 
          else
              print *, "Nombre de usuario o contraseña incorrectos."
          end if
      end if
  end subroutine login

   subroutine user_menu(username)
      character(len=20), intent(in) :: username
      integer :: choiceU = 0
      do  
         print *, "Bienvenido, ", username
         print *, "1. Visualizar reportes de las estructuras"
         print *, "2. Navegacion y gestion de imagenes"
         print *, "3. Opciones de carga masiva"
         print *, "4. Cerrar sesion"
         print *, "Seleccione una opcion:"
         read *, choiceU
         select case (choiceU)
         case (1)
            call reports_menu(username)
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

   subroutine reports_menu(username)
      character(len=20), intent(in) :: username
      integer :: choiceR = 0
      type(pixel), dimension(:), allocatable :: pixelsV
      integer :: idCapa, iC
      character(:), allocatable :: idCapaS
      do  
         print *, "----------", trim(username), "----------"
         print *, "1. Ver Arbol de Imagenes"
         print *, "2. Ver Arbol de Capas"
         print *, "3. Ver listado de Albumes"
         print *, "4. Ver Capa en la matriz dispersa"
         print *, "5. Salir"
         print *, "Seleccione una opcion:"
         read *, choiceR
         select case (choiceR)
         case (1)
            call system("start D:\EDD_PROYECTO2_202004071\executable\GraficaAVL.png")
         case (2)
            call system("start D:\EDD_PROYECTO2_202004071\executable\ABBgraph.png")
         case (3)
            call system("start D:\EDD_PROYECTO2_202004071\executable\AlbumesGraph.dot.png")
         case (4)
            call matrixD%init()
            print *, "Ingrese el id de la capa a visualizar:"
            read *, idCapa
            pixelsV = treeABB%getPixelsById(idCapa)
            do iC = 1, size(pixelsV)
               print *, "Fila: ", pixelsV(iC)%Fila, " Columna: ", pixelsV(iC)%Columna, " Color: ", pixelsV(iC)%color
            end do            
            write (idCapaS, '(I10)') idCapa
            call matrixD%create_dot("Capa"//trim(idCapaS))
         case (5)
            return
         case default
            print *, "Opcion Invalida."
         end select
      end do
   end subroutine

   subroutine mass_upload_menu(username)
      character(len=20), intent(in) :: username
      integer :: choiceUM = 0
 
      do 
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
      integer :: idimg, capas_size, iPixel
      integer :: size, iJ, iCapa, idImagenFijo
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
      do iJ = 1, size
         call jsonc%get_child(list_p, iJ, imagen_p, found)
         call jsonc%get_child(imagen_p, 'id', attribute_p, found=found)
         if (found) call jsonc%get(attribute_p, idimg)
         print *, "ID Imagen: ", idimg
         idImagenFijo = idimg
         write (contadorImg, '(I10)') idimg

         !call matrixD%init()
         call treeAVL%insert(idimg)
         call jsonc%get_parent(capas_array_p, attribute_p)
         call jsonc%get_child(imagen_p, 'capas', capas_array_p, found=found)
         call jsonc%info(capas_array_p, n_children=capas_size)
         do iCapa = 1, capas_size
            call jsonc%get_child(capas_array_p, iCapa, attribute_p, found)
            if (found) call jsonc%get(attribute_p, idimg)
            print *, "ID Capa: ", idimg
            !call treeABB%printPixelsById(idimg)
            !aca con el id de la capa, buscar en el arbol de capas y obtener los pixeles de esa capa para crear la imagen
            !pixelsI = treeABB%getPixelsById(idimg)
            !recorrer este arreglo de pixeles y añadirlo al otro arreglo de pixelesV

            call treeAVL%insertIntoABB(idImagenFijo, idimg)

         end do
         !call matrixD%create_dot(trim(contadorImg))

      end do
      !call treeAVL%printAVLandABB()
      call treeAVL%generateGraph()
      call json%destroy()
   end subroutine

   subroutine cargar_albumes(username)
      use json_module
      implicit none
      type(json_file) :: json
      type(json_value), pointer :: list_p, album_p, attribute_p, nombre_p, imags_array_p, imagen_p
      type(json_core) :: jsonc
      integer :: idimg, imags_size
      character(len=20), intent(in) :: username
      character(:), allocatable :: nombre_album
      integer :: size, iJ, iImg
      logical :: found
      call jsonc%initialize()
      call json%load(filename="D:\EDD_PROYECTO2_202004071\jsonFiles\albumes.json")
      call json%info('', n_children=size)
      call json%get_core(jsonc)
      call json%get('', list_p, found)
      do iJ = 1, size
         call jsonc%get_child(list_p, iJ, album_p, found)
         call jsonc%get_child(album_p, 'nombre_album', nombre_p, found=found)
         if (found) call jsonc%get(nombre_p, nombre_album)
         print *, "Nombre Album: ", trim(nombre_album)

         call jsonc%get_child(album_p, 'imgs', imags_array_p, found=found)
         call jsonc%info(imags_array_p, n_children=imags_size)
         do iImg = 1, imags_size
            call jsonc%get_child(imags_array_p, iImg, imagen_p, found)
            if (found) call jsonc%get(imagen_p, idimg)
            print *, "ID Imagen: ", idimg
            call listAlbums%insert(nombre_album, idimg)
         end do
      end do
      call listAlbums%printList()
      call listAlbums%graphDot("AlbumesGraph.dot")

   end subroutine

   subroutine cargar_capas(username)
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
      integer :: size, iJ, size2, iCC
      logical :: found
      type(pixel), dimension(:), allocatable :: pixels
      type(pixel) :: pixel_tmp

      !call matrixD%init()
      call jsonc%initialize()
      call json%load(filename="D:\EDD_PROYECTO2_202004071\jsonFiles\capas.json")
      call json%info('', n_children=size)
      call json%get_core(jsonc)
      call json%get('', list_p, found)
      do iCC = 1, size
         call jsonc%get_child(list_p, iCC, capa_p, found)
         call jsonc%get_child(capa_p, 'id_capa', attribute_p, found=found)
         if (found) call jsonc%get(attribute_p, id_capa)
         call jsonc%get_child(capa_p, 'pixeles', pixeles_p, found=found)
         call jsonc%info(pixeles_p, n_children=size2)
         !print *, "ID Capa: ", id_capa
         !print *, "Pixeles: ", size2
         !añadir el id_capa al arbol binario de busqueda
         allocate (pixels(size2))
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
            !print *, "Fila: ", fila, " Columna: ", columna, " Color: ", color
         end do
         call treeABB%insert(id_capa, pixels)
         deallocate (pixels)

      end do
      !call treeABB%printPixels()
      call treeABB%graph("ABBgraph")
      !call matrixD%create_dot()

   end subroutine

   subroutine image_menu(username)
      character(len=20), intent(in) :: username
      integer :: choiceImgM = 0
      print *, "USUARIO: ", username
      do   
         print *, "Menu de imagenes"
         print *, "1. Generacion de Imagenes"
         print *, "2. Gestion de Imagenes"
         print *, "3. Reportes de las estructuras"
         print *, "4. Salir"
         print *, "Seleccione una opcion:"
         read *, choiceImgM
         select case (choiceImgM)
         case (1)
            call generate_image_menu(username)
         case (2)
            !Para eliminar o añaadir 
            call manage_image_menu(username)
         case (3)
            call reportes(username)
         case (4)
            return
         case default
            print *, "Opcion Invalida."
         end select
      end do
   end subroutine

   subroutine reportes(username)
      character(len=20), intent(in) :: username
      integer :: choiceRep = 0
      print *, "USUARIO: ", username
      do
         print *, "1. Top 5 imagenes con mas capas"
         print *, "2. Todas las capas que son hojas"
         print *, "3. Profundidad del arbol de capas"
         print *, "4. Lista de capas en Preorden, Inorden y Postorden"
         print *, "5. Salir"
         print *, "Seleccione una opcion:"
         read *, choiceRep
         select case (choiceRep)
         case (1)
            call treeAVL%CantidadABBenAVL()
         case (2)
            call treeABB%printLeafNodes()
         case (3)
            !call treeABB%depth()
         case (4)
            print *, "---Lista de capas---"
            print *, "Preorden"
            call treeABB%preorder()
            print *, "Inorden"
            call treeABB%inorder()
            print *, "Postorden"
            call treeABB%posorder()
            print *, "-------------------"
         case (5)
            return
         case default
            print *, "Opcion Invalida."
         end select

      end do
   end subroutine

   subroutine manage_image_menu(username)
      character(len=20), intent(in) :: username
      integer :: choiceIMM = 0, idImagen,IdcapasA, numCapas, iA
      
      print *, "USUARIO: ", username
      do 
         print *, "1. Eliminar Imagen"
         print *, "2. Agregar Imagen a AVL"
         print *, "3. Salir"
         print *, "Seleccione una opcion:"
         read *, choiceIMM
         select case (choiceIMM)
         case (1)
            print *, "Ingrese el id de la imagen a eliminar:"
            read *, idImagen
            call treeAVL%delete(idImagen)
            call treeAVL%generateGraph()
         case (2)
            print *, "Ingrese el id de la imagen a agregar:"
            read *, idImagen
            call treeAVL%insert(idImagen)

            print *, "Ingrese el número de capas de la imagen:"
            read *, numCapas
            do iA = 1, numCapas
                print *, "Ingrese el id de la capa ", iA, ":"
                read *, IdcapasA
                call treeAVL%insertIntoABB(idImagen, IdcapasA)
            end do
            call treeAVL%generateGraph()           
         case (3)
            exit
         case default
            print *, "Opcion Invalida."
         end select
      end do
   
   end subroutine
      

   subroutine generate_image_menu(username)
      character(len=20), intent(in) :: username
      integer :: choiceG = 0
      print *, "USUARIO: ", username
      do while (choiceG /= 4)
         print *, "Generacion de Imagenes"
         print *, "1. Generar Imagen Por recorrido Limitado"
         print *, "2. Generar Imagen Por Arbol de imagenes"
         print *, "3. Generar imagen Ingresando el Id de las capas a usar"
         print *, "4. Salir"
         print *, "Seleccione una opcion:"
         read *, choiceG
         select case (choiceG)
         case (1)
            call generate_image_by_limitedTraversal(username)
         case (2)
            call generate_image_by_tree(username)
         case (3)
            call generate_image_by_cape_ids(username)
         case (4)
            exit
         case default
            print *, "Opcion Invalida."
         end select
      end do
   end subroutine

  
   subroutine generate_image_by_tree(username)
      integer, dimension(:), allocatable :: datosAA
      character(len=20), intent(in) :: username
      integer :: idImagen, IV, I
      character(len=:), allocatable ::  datosCapasT
      !Nombre del archivo de la imagen
      character(len=20) :: nombreImagen
      type(pixel), dimension(:), allocatable :: pixelesA
      !Buscar este idImagen en el arbol avl y obtener las capas que contiene en recorrido por amplitud
      !luego obtener los pixeles de estas capas y crear la imagen
      print *, "USUARIO: ", username
      print *, "Ingrese el id de la imagen a generar:"
      read *, idImagen
      datosCapasT = treeAVL%getABBValues(idImagen)
      datosCapasT = datosCapasT(2:)
      print *, "Orden de las capas por Amplitud: ", datosCapasT
      datosAA = DatosCapas(datosCapasT)

      call matrixD%init()
      do IV = 1, size(datosAA)
         print *, datosAA(IV)
         pixelesA = treeABB%getPixelsById(datosAA(IV))
         do I = 1, size(pixelesA)
            call matrixD%add(pixelesA(I)%Fila, pixelesA(I)%Columna, pixelesA(I)%color)
         end do
      end do
      print *, "Con que nombre desea guardar la imagen?"
      read *, nombreImagen
      call matrixD%create_dot(trim(nombreImagen))
   end subroutine

   subroutine generate_image_by_cape_ids(username)
      character(len=20), intent(in) :: username
      character(len=20) :: nombreImagen
      integer :: idCapas = 0, iV
      type(pixel), dimension(:), allocatable :: pixelsI
      !Aca recibir y leer los ids luego usar getCapeById para obtener los pixeles de estas capas
      !y crear la imagen, No se todavia como implementar esto para varios usuarios y que no se mezclen los datos
      !ya que datos asi como las capas no se pueden compartir, pero el usuario al volver a iniciar sesion
      !puede ver SUS imagenes, Sus capas, etc.
      call matrixD%init()
      print *, "USUARIO: ", username
      do  
         print *, "1. Ingresar ID de la Capa"
         print *, "2. Salir"
         print *, "Seleccione una opcion:"
         read *, iV
         select case (iV)
         case (1)
            print *, "Ingrese el id de la capa:"
            read *, idCapas
            pixelsI = treeABB%getPixelsById(idCapas)
            do iV = 1, size(pixelsI)
               ! print *, "Fila: ", pixelsI(iV)%Fila, " Columna: ", pixelsI(iV)%Columna, " Color: ", pixelsI(iV)%color
               call matrixD%add(pixelsI(iV)%Fila, pixelsI(iV)%Columna, pixelsI(iV)%color)
            end do
         case (2)
            exit
         case default
            print *, "Opcion Invalida."
         end select
      end do
      print *, "Con que nombre desea guardar la imagen?"
      read *, nombreImagen
      call matrixD%create_dot(trim(nombreImagen))

   end subroutine

   subroutine generate_image_by_limitedTraversal(username)
      character(len=20), intent(in) :: username
      integer :: idImagen, iPre = 0, cantidadCapas, iPixelPre, iPre1
      integer, dimension(:), allocatable :: datosA, datosIn
      character(:), allocatable :: datos, datosInorder, datosPos
      type(pixel), dimension(:), allocatable :: pixelsI, pixelsI2

      do while (iPre /= 4)
         print *, "USUARIO: ", username
         print *, "Ingrese que recorrido desea hacer:"
         print *, "1. Preorden"
         print *, "2. Inorden"
         print *, "3. Postorden"
         print *, "4. Salir"
         print *, "Seleccione una opcion:"
         read *, idImagen
         print *, "Ingrese la cantidad de capas a utilizar:"
         read *, cantidadCapas
         select case (idImagen)
         case (1)
            call treeABB%preorder()
            datos = treeABB%DatosPreoder()
            datos = datos(2:)
            !print *, "Datos: ", datos
            datosA = DatosCapas(datos)
            !do iPre = 1, size(datosA)
            ! print *, datosA(iPre)
            !end do
            call matrixD%init()
            do iPre = 1, cantidadCapas
               print *, datosA(iPre)
               !Buscar los ids de estas capas y obtener los pixeles de estos en un arreglo de pixeles
               !Luego mandar a llamar la funcion para crear la imagen
               pixelsI = treeABB%getPixelsById(datosA(iPre))
               do iPixelPre = 1, size(pixelsI)
                  ! print *, "Fila: ", pixelsI(iPixelPre)%Fila, " Columna: ", pixelsI(iPixelPre)%Columna, &
                  !  " Color: ", pixelsI(iPixelPre)%color
                  call matrixD%add(pixelsI(iPixelPre)%Fila, pixelsI(iPixelPre)%Columna, pixelsI(iPixelPre)%color)
               end do
            end do
            call matrixD%create_dot("ImagenPreorden")
         case (2)
            call matrixD%init()
            call treeABB%inorder()
            datosInorder = treeABB%DatosInorder()
            datosInorder = datosInorder(2:)
            datosA = DatosCapas(datosInorder)
            do iPre1 = 1, cantidadCapas
               print *, datosA(iPre1)
               pixelsI2 = treeABB%getPixelsById(datosA(iPre1))
               do iPixelPre = 1, size(pixelsI2)
                  call matrixD%add(pixelsI2(iPixelPre)%Fila, pixelsI2(iPixelPre)%Columna, pixelsI2(iPixelPre)%color)
               end do!FUNCIONA TODO SOLO QUE NO EJECUTAR CON EL DEBUGGER, POR QUE SE BUGEA AL CREAR EL DOT POR EL COMANDO, ENTONCES EJECUTARLO DIRECTAMENTE EL MAIN.EXE CON POWERSHELL

            end do

            call matrixD%create_dot("ImagenInorden")
         case (3)
            call matrixD%init()
            call treeABB%posorder()
            datosPos = treeABB%DatosPosorder()
            datosPos = datosPos(2:)

            print *, "Datos: ", datosPos
            datosA = DatosCapas(datosPos)
            do iPre1 = 1, cantidadCapas
               print *, datosA(iPre1)
               pixelsI2 = treeABB%getPixelsById(datosA(iPre1))
               do iPixelPre = 1, size(pixelsI2)
                  call matrixD%add(pixelsI2(iPixelPre)%Fila, pixelsI2(iPixelPre)%Columna, pixelsI2(iPixelPre)%color)
               end do
            end do
            call matrixD%create_dot("ImagenPostorden")
         case default
            print *, "Opcion Invalida."
         end select
      end do
   end subroutine

   function DatosCapas(str) result(datosIDS)
      implicit none
      character(len=*) :: str
      integer, dimension(:), allocatable :: datosIDS
      character(len=10), dimension(:), allocatable :: strArray
      integer :: i, n, count

      count = 0
      do i = 1, len_trim(str)
         if (str(i:i) == ',') count = count + 1
      end do

      n = count + 1
      allocate (datosIDS(n))
      allocate (strArray(n))

      call split(str, ',', strArray)

      do i = 1, n
         read (strArray(i), *) datosIDS(i)
      end do
   end function DatosCapas

   subroutine split(str, delimiter, strArray)
      implicit none
      character(len=*), intent(in) :: str, delimiter
      character(len=10), dimension(:), allocatable, intent(out) :: strArray
      integer :: i, n, start, end, count

      count = 0
      do i = 1, len_trim(str)
         if (str(i:i) == ',') count = count + 1
      end do

      n = count + 1
      allocate (strArray(n))

      start = 1
      do i = 1, n
         end = index(str(start:), delimiter)
         if (end > 0) then
            strArray(i) = str(start:start + end - 2)
            start = start + end
         else
            strArray(i) = str(start:)
         end if
      end do
   end subroutine split

   subroutine admin_menu()
      integer :: choiceA = 0
      integer(kind=8) :: idEliminar
    
      do  
         print *, "1. Cargar usuarios"
         print *, "2. Arbol B de usuarios"
         print *, "3. Modificar usuarios"
         print *, "4. Eliminar usuarios"
         print *, "5. Salir"
         print *, "Seleccione una opcion:"
         read *, choiceA
         select case (choiceA)
         case (1)
            call add_user()
         case (2)
            call system("start D:\EDD_PROYECTO2_202004071\executable\GraficaArbolB.png")
            print *, "Abriendo arbol B..."
         case (3)
            call modify_user()
         case (4)
            print *, "Eliminar usuarios"
            print * , "Ingrese el DPI del usuario a eliminar"
            read *,  idEliminar
             
            !call deleteUserFromBTree(idEliminar)

            print *, "Actualizando arbol B..."

            call traversal(root) 
            call writeGraphviz(root,"GraficaArbolB.dot")
            call system("dot -Tpng D:\EDD_PROYECTO2_202004071\executable\GraficaArbolB.dot&
            & -o D:\EDD_PROYECTO2_202004071\executable\GraficaArbolB.png")
            
       
         case (5)
            return
            
         case default
            print *, "Opcion Invalida."
         end select
      end do
   end subroutine

   subroutine modify_user()
      type(UsuarioB) :: usuarioMod
      integer(kind=8) :: idModificar
      character(len=50) :: nombreNuevo, contrasenaNueva
      print *, "Modificar usuario ----"
      print *, "Ingrese el DPI del usuario a modificar: "
      read *, idModificar
      print *, "Ingrese el nuevo nombre: "
      read *, nombreNuevo
      print *, "Ingrese la nueva contraseña: "
      read *, contrasenaNueva
      usuarioMod%id = idModificar
      usuarioMod%nombre = nombreNuevo
      usuarioMod%contrasena = contrasenaNueva
      call updateB(usuarioMod)
      print *, "Usuario modificado con exito."
      print *, "Actualizando arbol B..."
      call traversal(root)
      call writeGraphviz(root,"GraficaArbolB.dot")
      call system("dot -Tpng D:\EDD_PROYECTO2_202004071\executable\GraficaArbolB.dot&
      & -o D:\EDD_PROYECTO2_202004071\executable\GraficaArbolB.png")       
   end subroutine

   subroutine add_user()
      use json_module
      implicit none
      type(UsuarioB) :: usuarioAB
      type(json_file) :: json
      type(json_value), pointer :: list_p, client_p, attribute_p
      type(json_core) :: jsonc
      character(:), allocatable :: dpi, nombre_cliente, password
      integer :: size, iJ 
      logical :: found
      integer(kind = 8)  :: dpiN
      call jsonc%initialize()
      call json%load(filename="D:\EDD_PROYECTO2_202004071\jsonFiles\usuarios.json")
      call json%info('', n_children=size)
      call json%get_core(jsonc)
      call json%get('', list_p, found)
      do iJ = 1, size

         call jsonc%get_child(list_p, iJ, client_p, found)
         call jsonc%get_child(client_p, 'nombre_cliente', attribute_p, found=found)
 
         if (found) call jsonc%get(attribute_p, nombre_cliente)
         call jsonc%get_child(client_p, 'dpi', attribute_p, found=found)
 
         if (found) call jsonc%get(attribute_p, dpi)
         call jsonc%get_child(client_p, 'password', attribute_p, found=found)
         if (found) call jsonc%get(attribute_p, password)
         print *, "Nombre: ", trim(nombre_cliente), " DPI: ", trim(dpi), " Password: ", trim(password)

         read(dpi,*) dpiN
         
         usuarioAB%id = dpiN
         usuarioAB%nombre = nombre_cliente
         usuarioAB%contrasena = password
         
         call insertB(usuarioAB)

         usuarioAB%id = 0
         usuarioAB%nombre = ""
         usuarioAB%contrasena = ""
         !Limpiar variables para evitar errores


      end do
      call json%destroy()
      print *, "Usuarios cargados"
 

      call traversal(root)
      print *, ""

      call writeGraphviz(root,"GraficaArbolB.dot")
      call system("dot -Tpng D:\EDD_PROYECTO2_202004071\executable\GraficaArbolB.dot& 
      & -o D:\EDD_PROYECTO2_202004071\executable\GraficaArbolB.png")

   end subroutine

end program main
 