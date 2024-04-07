module abb_m

   implicit none
   private

   type, public :: pixel
      integer :: Fila
      integer :: Columna
      character(:), allocatable :: Color !aca se guardara el valor hexadecimal del color por ejemplo #F0F0F0
   end type pixel

   type :: Node_t
      integer :: value !el nodo tendra un id que seria el id de la capa y un arreglo de pixeles dinamico
      type(pixel), dimension(:), allocatable :: pixels
      type(Node_t), pointer :: right => null()
      type(Node_t), pointer :: left => null()
   end type Node_t

   type, public :: abb
      type(Node_t), pointer :: root => null()

   contains
      procedure :: insert
      procedure :: delete
      procedure :: preorder
      procedure :: DatosPreoder
      procedure :: inorder
      procedure :: DatosInorder
      procedure :: posorder
      procedure :: DatosPosorder
      procedure :: graph
      procedure :: printPixels
      procedure :: printPixelsById
      procedure :: getPixelsById
      procedure :: getPixelSizeByID
      procedure :: printLeafNodes
      procedure :: print_tree_depth
      
   end type abb

contains

recursive function get_depth(root) result(depth)
    type(Node_t), pointer, intent(in) :: root
    integer :: depth

    if (.not. associated(root)) then
        depth = 0
    else
        depth = 1 + max(get_depth(root%left), get_depth(root%right))
    end if
end function get_depth

subroutine print_tree_depth(self)
    class(abb), intent(in) :: self
    integer :: depth

    depth = get_depth(self%root)
    write (*, '("Profundidad del arbol de capas: ", I0)') depth
end subroutine print_tree_depth

subroutine printLeafNodes(self)
   class(abb), intent(in) :: self
   
   call printLeafNodesRec(self%root)
end subroutine printLeafNodes

recursive subroutine printLeafNodesRec(root)
   type(Node_t), pointer, intent(in) :: root
   
   if (associated(root)) then
       if (.not. associated(root%left) .and. .not. associated(root%right)) then
           ! Este es un nodo hoja
           write (*, '(I0)') root%value
       else
           ! Recorrer el subárbol izquierdo
           call printLeafNodesRec(root%left)
           
           ! Recorrer el subárbol derecho
           call printLeafNodesRec(root%right)
       end if
   end if
end subroutine printLeafNodesRec

   subroutine printPixelsById(self, id)
      class(abb), intent(in) :: self
      integer, intent(in) :: id
      type(Node_t), pointer :: node

      node => searchNodeRec(self%root, id)
      if (associated(node)) then
         call printPixelsOfNode(node)
      else
         write (*, *) "No se encontro el nodo con el id ", id
      end if
   end subroutine printPixelsById

   recursive function searchNodeRec(root, id) result(res)
      type(Node_t), pointer, intent(in) :: root
      integer, intent(in) :: id
      type(Node_t), pointer :: res
      if (.not. associated(root)) then
         res => null()
      else if (id == root%value) then
         res => root
      else if (id < root%value) then
         res => searchNodeRec(root%left, id)
      else
         res => searchNodeRec(root%right, id)
      end if
   end function searchNodeRec

   subroutine printPixelsOfNode(node)
      type(Node_t), pointer, intent(in) :: node
      integer :: i

      !Aca ir añadiendo esto a la matriz dispersa dadas las capas

      if (associated(node)) then
         write (*, *) "Node value: ", node%value
         do i = 1, size(node%pixels)
            write (*, *) "Pixel ", i, ": Fila ", node%pixels(i)%Fila, &
               ", Columna ", node%pixels(i)%Columna, ", Color ", node%pixels(i)%Color
         end do
      end if
   end subroutine printPixelsOfNode

   subroutine printPixels(self)
      class(abb), intent(in) :: self
      call printPixelsRec(self%root)
   end subroutine printPixels

   recursive subroutine printPixelsRec(root)
      type(Node_t), pointer, intent(in) :: root
      integer :: i

      if (associated(root)) then
         write (*, *) "Node value: ", root%value
         do i = 1, size(root%pixels)
            write (*, *) "Pixel ", i, ": Fila ", root%pixels(i)%Fila, &
               ", Columna ", root%pixels(i)%Columna, ", Color ", root%pixels(i)%Color
         end do
         call printPixelsRec(root%left)
         call printPixelsRec(root%right)
      end if
   end subroutine printPixelsRec
   !Subrutinas del tipo abb
   subroutine insert(self, val, pixels)
      class(abb), intent(inout) :: self
      type(pixel), dimension(:), intent(inout) :: pixels
      integer, intent(in) :: val

      if (.not. associated(self%root)) then
         allocate (self%root)
         self%root%value = val
         if (allocated(self%root%pixels)) deallocate (self%root%pixels)
         allocate (self%root%pixels(size(pixels)))
         self%root%pixels = pixels
      else
         call insertRec(self%root, val, pixels)
      end if
   end subroutine insert

   function getPixelsById(self, id) result(pixels)
      class(abb), intent(in) :: self
      integer, intent(in) :: id
      type(Node_t), pointer :: node
      type(pixel), dimension(:), allocatable :: pixels

      node => searchNodeRec(self%root, id)
      if (associated(node)) then
         pixels = node%pixels
      else
         write (*, *) "No se encontro el nodo con el id ", id
      end if
   end function getPixelsById

   function getPixelSizeByID(self, id) result(sizeD)
      class(abb), intent(in) :: self
      integer, intent(in) :: id
      type(Node_t), pointer :: node
      integer :: sizeD

      node => searchNodeRec(self%root, id)
      if (associated(node)) then
         sizeD = size(node%pixels)
      else
         write (*, *) "No se encontro el nodo con el id ", id
         sizeD = -1
      end if
   end function getPixelSizeByID

   recursive subroutine insertRec(root, val, pixels)
      type(Node_t), pointer, intent(inout) :: root
      type(pixel), dimension(:), intent(inout) :: pixels
      integer, intent(in) :: val

      if (val < root%value) then
         if (.not. associated(root%left)) then
            allocate (root%left)
            root%left%value = val
            root%left%pixels = pixels
         else
            call insertRec(root%left, val, pixels)
         end if
      else if (val > root%value) then
         if (.not. associated(root%right)) then
            allocate (root%right)
            root%right%value = val
            root%right%pixels = pixels
         else
            call insertRec(root%right, val, pixels)
         end if
      end if
   end subroutine insertRec

   subroutine delete(self, val)
      class(abb), intent(inout) :: self
      integer, intent(inout) :: val

      self%root => deleteRec(self%root, val)
   end subroutine delete
   recursive function deleteRec(root, value) result(res)
      type(Node_t), pointer :: root
      integer, intent(in) :: value
      type(Node_t), pointer :: res
      type(Node_t), pointer :: temp

      if (.not. associated(root)) then
         res => root
         return
      end if

      if (value < root%value) then
         root%left => deleteRec(root%left, value)
      else if (value > root%value) then
         root%right => deleteRec(root%right, value)
      else
         if (.not. associated(root%left)) then
            temp => root%right
            deallocate (root)
            res => temp
            return
         else if (.not. associated(root%right)) then
            temp => root%left
            deallocate (root)
            res => temp
            return
         else
            call getMajorOfMinors(root%left, temp)
            root%value = temp%value
            root%left => deleteRec(root%left, temp%value)
         end if
      end if

      res => root
   end function deleteRec
   recursive subroutine getMajorOfMinors(root, major)
      type(Node_t), pointer :: root, major
      if (associated(root%right)) then
         call getMajorOfMinors(root%right, major)
      else
         major => root
      end if
   end subroutine getMajorOfMinors

   subroutine preorder(self)
      class(abb), intent(in) :: self

      call preorderRec(self%root)
      write (*, '()')
   end subroutine preorder

   recursive subroutine preorderRec(root)
      type(Node_t), pointer, intent(in) :: root

      if (associated(root)) then
         ! RAIZ - IZQ - DER
         write (*, '(I0 A)', advance='no') root%value, " - "
         call preorderRec(root%left)
         call preorderRec(root%right)
      end if
   end subroutine preorderRec

   !ncesito una funcion llamada datospreorder que me devuelva un string con los datos de preorder

   function DatosPreoder(self) result(datos)
      class(abb), intent(in) :: self
      character(len=1000) :: datos
      character(len=10) :: temp
      integer, allocatable :: array(:)
      integer :: i

      call DatosPreorderRec(self%root, array)

      datos = ''  ! Initialize the result string
      do i = 1, size(array)
         write (temp, '(I0)') array(i)  ! Convert the integer to a string
         datos = trim(datos)//','//trim(temp)  ! Concatenate the strings with a space in between
      end do
   end function DatosPreoder

   recursive subroutine DatosPreorderRec(root, array)
      type(Node_t), pointer, intent(in) :: root
      integer, allocatable, intent(inout) :: array(:)
      integer :: i

      if (associated(root)) then
         i = size(array) + 1
         if (allocated(array)) then
            array = [array, root%value]
         else
            allocate (array(1))
            array(1) = root%value
         end if
         call DatosPreorderRec(root%left, array)
         call DatosPreorderRec(root%right, array)
      end if
   end subroutine DatosPreorderRec

   subroutine inorder(self)
      class(abb), intent(in) :: self

      call inordenRec(self%root)
      print *, ""
   end subroutine inorder

   recursive subroutine inordenRec(root)
      type(Node_t), pointer, intent(in) :: root

      if (associated(root)) then
         ! IZQ - RAIZ - DER
         call inordenRec(root%left)
         write (*, '(I0 A)', advance='no') root%value, " - "
         call inordenRec(root%right)
      end if
   end subroutine inordenRec

   function DatosInorder(self) result(datos)
      class(abb), intent(in) :: self
      character(len=1000) :: datos
      character(len=10) :: temp
      integer, allocatable :: array(:)
      integer :: i

      call DatosInorderRec(self%root, array)

      datos = ''  ! Initialize the result string
      do i = 1, size(array)
         write (temp, '(I0)') array(i)  ! Convert the integer to a string
         datos = trim(datos)//','//trim(temp)  ! Concatenate the strings with a space in between
      end do
   end function DatosInorder

   recursive subroutine DatosInorderRec(root, array)
      type(Node_t), pointer, intent(in) :: root
      integer, allocatable, intent(inout) :: array(:)
      integer :: i

      if (associated(root)) then
         i = size(array) + 1
         if (allocated(array)) then
            array = [array, root%value]
         else
            allocate (array(1))
            array(1) = root%value
         end if
         call DatosInorderRec(root%left, array)
         call DatosInorderRec(root%right, array)
      end if
   end subroutine DatosInorderRec

   subroutine posorder(self)
      class(abb), intent(in) :: self

      call posordenRec(self%root)
      print *, ""
   end subroutine posorder

   recursive subroutine posordenRec(root)
      type(Node_t), pointer, intent(in) :: root

      if (associated(root)) then
         ! IZQ - DER - RAIZ
         call posordenRec(root%left)
         call posordenRec(root%right)
         write (*, '(I0 A)', advance='no') root%value, " - "
      end if
   end subroutine posordenRec

   function DatosPosorder(self) result(datos)
      class(abb), intent(in) :: self
      character(len=1000) :: datos
      character(len=10) :: temp
      integer, allocatable :: array(:)
      integer :: i

      call DatosPosorderRec(self%root, array)

      datos = ''  ! Initialize the result string
      do i = 1, size(array)
         write (temp, '(I0)') array(i)  ! Convert the integer to a string
         datos = trim(datos)//','//trim(temp)  ! Concatenate the strings with a space in between
      end do
   end function DatosPosorder

   recursive subroutine DatosPosorderRec(root, array)
      type(Node_t), pointer, intent(in) :: root
      integer, allocatable, intent(inout) :: array(:)
      integer :: i

      if (associated(root)) then
         i = size(array) + 1
         
         call DatosPosorderRec(root%left, array)
         call DatosPosorderRec(root%right, array)
         if (allocated(array)) then
            array = [array, root%value]
         else
            allocate (array(1))
            array(1) = root%value
         end if
      end if
   end subroutine DatosPosorderRec

   subroutine graph(self, filename)
      class(abb), intent(in) :: self
      character(len=*), intent(in) :: filename 
      character(len=:), allocatable :: dotStructure
      character(len=:), allocatable :: createNodes
      character(len=:), allocatable :: linkNodes
      character(len=100) :: command

      createNodes = ''
      linkNodes = ''

      dotStructure = "digraph G{"//new_line('a')
      dotStructure = dotStructure//"node [shape=circle];"//new_line('a')

      if (associated(self%root)) then
         call RoamTree(self%root, createNodes, linkNodes)
      end if

      dotStructure = dotStructure//trim(createNodes)//trim(linkNodes)//"}"//new_line('a')
      call write_dot(filename, dotStructure)
      command = "dot -Tpng " // trim(filename) // ".dot -o " // trim(filename) // ".png"
      call system(command)
      print *, "Archivo actualizado existosamente."
   end subroutine graph
   recursive subroutine RoamTree(current, createNodes, linkNodes)
      type(Node_t), pointer :: current
      character(len=:), allocatable, intent(inout) :: createNodes, linkNodes
      character(len=20) :: address, str_value

      if (associated(current)) then
         ! SE OBTIENE INFORMACION DEL NODO ACTUAL
         address = get_address_memory(current)
         write (str_value, '(I0)') current%Value
         createNodes = createNodes//'"'//trim(address)//'"'//'[label="'//trim(str_value)//'"];'//new_line('a')
         ! VIAJAMOS A LA SUBRAMA IZQ
         if (associated(current%Left)) then
            linkNodes = linkNodes//'"'//trim(address)//'"'//" -> "
            address = get_address_memory(current%Left)
            linkNodes = linkNodes//'"'//trim(address)//'" ' &
                        //'[label = "L"];'//new_line('a')

         end if
         ! VIAJAMOS A LA SUBRAMA DER
         if (associated(current%Right)) then
            address = get_address_memory(current)
            linkNodes = linkNodes//'"'//trim(address)//'"'//" -> "
            address = get_address_memory(current%Right)
            linkNodes = linkNodes//'"'//trim(address)//'" ' &
                        //'[label = "R"];'//new_line('a')
         end if

         call RoamTree(current%Left, createNodes, linkNodes)
         call RoamTree(current%Right, createNodes, linkNodes)
      end if
   end subroutine RoamTree
   subroutine write_dot(filename, code)
      character(len=*), intent(in) :: code, filename
      character(len=:), allocatable :: dot_filename, png_filename

      ! Agregar extensiones
      dot_filename = trim(filename)//".dot"
      png_filename = trim(filename)//".png"

      open (10, file=dot_filename, status='replace', action='write')
      write (10, '(A)') trim(code)
      close (10)

   end subroutine write_dot

   function get_address_memory(node) result(address)
      !class(matrix_t), intent(in) :: self
      type(Node_t), pointer :: node
      character(len=20) :: address
      ! integer 8
      integer*8 :: i

      i = loc(node) ! get the address of x
      ! convert the address to string
      write (address, 10) i
10    format(I0)

   end function get_address_memory

end module abb_m
