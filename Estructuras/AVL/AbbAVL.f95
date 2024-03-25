module abbavl
 
    implicit none
    private


    
    type,public :: pixelVL
        integer :: Fila
        integer :: Columna
        character(:), allocatable :: Color !aca se guardara el valor hexadecimal del color por ejemplo #F0F0F0
    end type pixelVL

    type :: Node_tVL
        integer :: value !el nodo tendra un id que seria el id de la capa y un arreglo de pixeles dinamico
        type(pixelVL), dimension(:), allocatable :: pixels
        type(Node_tVL), pointer :: right => null()
        type(Node_tVL), pointer :: left => null()
    end type Node_tVL

    type, public :: abbVL
        type(Node_tVL), pointer :: root => null()

    contains
        procedure :: insert
        procedure :: delete
        procedure :: preorder
        procedure :: inorder
        procedure :: posorder
        procedure :: graph
        procedure :: printPixels
        procedure :: printPixelsById
        procedure :: getPixelsById
        procedure :: getPixelSizeByID
    end type abbVL

contains    

subroutine printPixelsById(self, id)
    class(abbVL), intent(in) :: self
    integer, intent(in) :: id
    type(Node_tVL), pointer :: node
    
    node => searchNodeRec(self%root, id)
    if (associated(node)) then
        call printPixelsOfNode(node)
    else
        write(*, *) "No se encontro el nodo con el id ", id
    end if
end subroutine printPixelsById

recursive function searchNodeRec(root, id) result(res)
    type(Node_tVL), pointer , intent(in) :: root
    integer, intent(in) :: id
    type(Node_tVL), pointer :: res
    if(.not. associated(root)) then
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
    type(Node_tVL), pointer, intent(in) :: node
    integer :: i

    !Aca ir añadiendo esto a la matriz dispersa dadas las capas

    if (associated(node)) then
        write(*, *) "Node value: ", node%value
        do i = 1, size(node%pixels)
            write(*, *) "Pixel ", i, ": Fila ", node%pixels(i)%Fila,&
             ", Columna ", node%pixels(i)%Columna, ", Color ", node%pixels(i)%Color
        end do
    end if
end subroutine printPixelsOfNode

subroutine printPixels(self)
    class(abbVL), intent(in) :: self
    call printPixelsRec(self%root)
end subroutine printPixels

recursive subroutine printPixelsRec(root)
    type(Node_tVL), pointer, intent(in) :: root
    integer :: i

    if (associated(root)) then
        write(*, *) "Node value: ", root%value
        do i = 1, size(root%pixels)
            write(*, *) "Pixel ", i, ": Fila ", root%pixels(i)%Fila,&
             ", Columna ", root%pixels(i)%Columna, ", Color ", root%pixels(i)%Color
        end do
        call printPixelsRec(root%left)
        call printPixelsRec(root%right)
    end if
end subroutine printPixelsRec
    !Subrutinas del tipo abbVL
    subroutine insert(self, val,pixels)
        class(abbVL), intent(inout) :: self
        type(pixelVL), dimension(:), intent(inout) :: pixels
        integer, intent(in) :: val

        if (.not. associated(self%root)) then
            allocate(self%root)
            self%root%value = val
            if (allocated(self%root%pixels)) deallocate(self%root%pixels)
            allocate(self%root%pixels(size(pixels)))
            self%root%pixels = pixels
        else
            call insertRec(self%root, val,pixels)
        end if
    end subroutine insert

    function getPixelsById(self, id) result(pixels)
        class(abbVL), intent(in) :: self
        integer, intent(in) :: id
        type(Node_tVL), pointer :: node
        type(pixelVL), dimension(:), allocatable :: pixels
    
        node => searchNodeRec(self%root, id)
        if (associated(node)) then
            pixels = node%pixels
        else
            write(*, *) "No se encontro el nodo con el id ", id
        end if
    end function getPixelsById

    function getPixelSizeByID(self, id) result(sizeD)
        class(abbVL), intent(in) :: self
        integer, intent(in) :: id
        type(Node_tVL), pointer :: node
        integer :: sizeD
    
        node => searchNodeRec(self%root, id)
        if (associated(node)) then
            sizeD = size(node%pixels)
        else
            write(*, *) "No se encontro el nodo con el id ", id
            sizeD = -1
        end if
    end function getPixelSizeByID

    recursive subroutine insertRec(root, val,pixels)
        type(Node_tVL), pointer, intent(inout) :: root
        type(pixelVL), dimension(:), intent(inout) :: pixels
        integer, intent(in) :: val
        
        if (val < root%value) then
            if (.not. associated(root%left)) then
                allocate(root%left)
                root%left%value = val
                root%left%pixels = pixels
            else
                call insertRec(root%left, val,pixels)
            end if
        else if (val > root%value) then
            if (.not. associated(root%right)) then
                allocate(root%right)
                root%right%value = val
                root%right%pixels = pixels
            else
                call insertRec(root%right, val,pixels)
            end if
        end if
    end subroutine insertRec

    subroutine delete(self, val)
        class(abbVL), intent(inout) :: self
        integer, intent(inout) :: val
    
        self%root => deleteRec(self%root, val)
    end subroutine delete
    recursive function deleteRec(root, value) result(res)
        type(Node_tVL), pointer :: root
        integer, intent(in) :: value
        type(Node_tVL), pointer :: res
        type(Node_tVL), pointer :: temp

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
                deallocate(root)
                res => temp
                return
            else if (.not. associated(root%right)) then
                temp => root%left
                deallocate(root)
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
        type(Node_tVL), pointer :: root, major
        if (associated(root%right)) then
            call getMajorOfMinors(root%right, major)
        else
            major => root
        end if
    end subroutine getMajorOfMinors

    subroutine preorder(self)
        class(abbVL), intent(in) :: self
        
        call preorderRec(self%root)
        write(*, '()')
    end subroutine preorder
    recursive subroutine preorderRec(root)
        type(Node_tVL), pointer, intent(in) :: root

        if(associated(root)) then
            ! RAIZ - IZQ - DER
            write(*, '(I0 A)', advance='no') root%value, " - "
            call preorderRec(root%left)
            call preorderRec(root%right)
        end if
    end subroutine preorderRec

    subroutine inorder(self)
        class(abbVL), intent(in) :: self
        
        call inordenRec(self%root)
        print *, ""
    end subroutine inorder
    recursive subroutine inordenRec(root)
        type(Node_tVL), pointer, intent(in) :: root

        if(associated(root)) then
            ! IZQ - RAIZ - DER
            call inordenRec(root%left)
            write(*, '(I0 A)', advance='no') root%value, " - "
            call inordenRec(root%right)
        end if
    end subroutine inordenRec

    subroutine posorder(self)
        class(abbVL), intent(in) :: self
        
        call posordenRec(self%root)
        print *, ""
    end subroutine posorder
    recursive subroutine posordenRec(root)
        type(Node_tVL), pointer, intent(in) :: root

        if(associated(root)) then
            ! IZQ - DER - RAIZ
            call posordenRec(root%left)
            call posordenRec(root%right)
            write(*, '(I0 A)', advance='no') root%value, " - "
        end if
    end subroutine posordenRec

    subroutine graph(self, filename)
        class(abbVL), intent(in) :: self
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dotStructure
        character(len=:), allocatable :: createNodes
        character(len=:), allocatable :: linkNodes
        
        createNodes = ''
        linkNodes = ''

        dotStructure = "digraph G{" // new_line('a')
        dotStructure = dotStructure // "node [shape=circle];" // new_line('a')

        if (associated(self%root)) then
            call RoamTree(self%root, createNodes, linkNodes)
        end if
        
        dotStructure = dotStructure // trim(createNodes) // trim(linkNodes) // "}" // new_line('a')
        call write_dot(filename, dotStructure)
        print *, "Archivo actualizado existosamente."
    end subroutine graph
    recursive subroutine RoamTree(current, createNodes, linkNodes)
        type(Node_tVL), pointer :: current
        character(len=:), allocatable, intent(inout) :: createNodes, linkNodes
        character(len=20) :: address, str_value

        if (associated(current)) then
            ! SE OBTIENE INFORMACION DEL NODO ACTUAL
          address = get_address_memory(current)
          write(str_value, '(I0)') current%Value
          createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // trim(str_value) // '"];' // new_line('a')
          ! VIAJAMOS A LA SUBRAMA IZQ
          if (associated(current%Left)) then
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory(current%Left)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = "L"];' // new_line('a')
    
          end if
          ! VIAJAMOS A LA SUBRAMA DER
          if (associated(current%Right)) then
            address = get_address_memory(current)
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory(current%Right)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = "R"];' // new_line('a')
          end if
    
          call RoamTree(current%Left, createNodes, linkNodes)
          call RoamTree(current%Right, createNodes, linkNodes)
        end if
    end subroutine RoamTree
    subroutine write_dot(filename, code)
        character(len=*), intent(in) :: code, filename
        character(len=:), allocatable :: dot_filename, png_filename
        
        ! Agregar extensiones
        dot_filename = trim(filename) // ".dot"
        png_filename = trim(filename) // ".png"
        
        open(10, file=dot_filename, status='replace', action='write')
        write(10, '(A)') trim(code)
        close(10)

 
    end subroutine write_dot

    function get_address_memory(node) result(address)
        !class(matrix_t), intent(in) :: self
        type(Node_tVL), pointer :: node
        character(len=20) :: address
        ! integer 8
        integer*8 :: i
    
        i = loc(node) ! get the address of x
        ! convert the address to string
        write(address, 10) i 
        10 format(I0)
    
    end function get_address_memory

end module abbavl
