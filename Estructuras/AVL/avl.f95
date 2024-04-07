module TreeAVL_M
   use abbavl
   use LinkedListModule
   implicit none

   ! Cons
   integer, parameter :: LEFT_HEAVY = -1
   integer, parameter :: BALANCED = 0
   integer, parameter :: RIGHT_HEAVY = +1


   type(LinkedList) :: ListaL
 


   type Node_tAVL
      integer :: Value
      integer :: Factor
 
      type(NodoABB) :: NodoABB
      type(Node_tAVL), pointer :: Left => null()
      type(Node_tAVL), pointer :: Right => null()
   end type Node_tAVL

   

   type Tree_t
      type(Node_tAVL), pointer :: root => null() 
   contains
      procedure :: newTree
      procedure :: insert
      procedure :: generateGraph
      procedure :: insertIntoABB
      procedure :: printAVLandABB
      procedure :: getABBValues
      procedure :: delete
      procedure :: CantidadABBenAVL
      procedure :: GenerateGraph2


   end type Tree_t

contains


subroutine CantidadABBenAVL(self)
   class(Tree_t), intent(inout) :: self
   type(LinkedList) :: ListaAux
   call printCantidadABBenAVLRec(self%root, ListaAux)
   call SortList(ListaAux)
   call PrintList(ListaAux)
end subroutine CantidadABBenAVL

recursive subroutine printCantidadABBenAVLRec(Anode, ListaAux)
   type(LinkedList), intent(inout) :: ListaAux
   type(Node_tAVL), pointer :: Anode

   if (associated(Anode)) then
      !print *, "AVL Node: ", Anode%Value
      !print *, "Cantidad de nodos en ABB: ", Anode%NodoABB%countNodes()
      call AddNode(ListaAux, Anode%Value,Anode%NodoABB%countNodes())
      call printCantidadABBenAVLRec(Anode%Left, ListaAux)
      call printCantidadABBenAVLRec(Anode%Right, ListaAux)
   end if
end subroutine printCantidadABBenAVLRec

 

recursive subroutine delete(self, value)
class(Tree_t), intent(inout) :: self
integer, intent(in) :: value
type(Node_tAVL), pointer :: nodeToDelete, replacementNode

self%root => deleteNode(self%root, value, nodeToDelete, replacementNode)
end subroutine delete

recursive function deleteNode(root, value, nodeToDelete, replacementNode) result(newRoot)
    type(Node_tAVL), pointer :: root, newRoot, nodeToDelete, replacementNode
    integer, intent(in) :: value

    if (.not. associated(root)) then
        newRoot => null()
        nodeToDelete => null()
        replacementNode => null()
    else if (value < root%Value) then
        root%Left => deleteNode(root%Left, value, nodeToDelete, replacementNode)
        newRoot => rebalance(root, replacementNode)
    else if (value > root%Value) then
        root%Right => deleteNode(root%Right, value, nodeToDelete, replacementNode)
        newRoot => rebalance(root, replacementNode)
    else
        nodeToDelete => root
        if (.not. associated(root%Left)) then
            newRoot => root%Right
            replacementNode => null()
        else if (.not. associated(root%Right)) then
            newRoot => root%Left
            replacementNode => null()
        else
            root%Right => findMinNode(root%Right, replacementNode)
            root%Value = replacementNode%Value
            newRoot => rebalance(root, replacementNode)
        end if
    end if
end function deleteNode

recursive function findMinNode(root, minNode) result(newRoot)
type(Node_tAVL), pointer :: root, newRoot, minNode
if (.not. associated(root%Left)) then
    newRoot => root%Right
    minNode => root
else
    root%Left => findMinNode(root%Left, minNode)
    newRoot => rebalance(root, minNode)
end if
end function findMinNode

function rebalance(node, replacementNode) result(newNode)
type(Node_tAVL), pointer :: node, newNode, replacementNode
integer :: balanceFactor
!No se por que se autobalancea sin hacer nada aca, pero si se hace el rebalanceo asi que mejor no tocarlo
!Seguro es por la manera en la que ya hace las rotaciones al igual con solo insertar
end function rebalance


   function getABBValues(self, Id) result(values)
      class(Tree_t), intent(in) :: self
      integer, intent(in) :: Id
      character(len=:), allocatable :: values
      type(Node_tAVL), pointer :: avlNode

      avlNode => findAVLNode(self%root, Id)
      if (associated(avlNode)) then
         values = avlNode%NodoABB%amplitudData()
      else
         print *, "AVL node not found."
      end if
   end function getABBValues


   subroutine printAVLandABB(self)
      class(Tree_t), intent(inout) :: self

      call printAVLandABBRec(self%root)
   end subroutine printAVLandABB

   recursive subroutine printAVLandABBRec(Anode)
      type(Node_tAVL), pointer :: Anode

      if (associated(Anode)) then
         print *, "AVL Node: ", Anode%Value
         print *, "ABB in AVL Node: "
         call Anode%NodoABB%amplitud()
         call printAVLandABBRec(Anode%Left)
         call printAVLandABBRec(Anode%Right)
      end if
   end subroutine printAVLandABBRec

   subroutine insertIntoABB(self, avlValue, abbValue)
      class(Tree_t), intent(inout) :: self
      integer, intent(in) :: avlValue, abbValue
      type(Node_tAVL), pointer :: avlNode

      avlNode => findAVLNode(self%root, avlValue)

      if (associated(avlNode)) then
         call avlNode%NodoABB%insert(abbValue)
      else
         print *, "AVL node not found."
      end if
   end subroutine insertIntoABB

   recursive function findAVLNode(root, value) result(node)
      type(Node_tAVL), pointer :: root, node
      integer, intent(in) :: value

      if (.not. associated(root)) then
         node => null()
      else if (value == root%Value) then
         node => root
      else if (value < root%Value) then
         node => findAVLNode(root%Left, value)
      else
         node => findAVLNode(root%Right, value)
      end if
   end function findAVLNode

   function NewNode(value) result(nodePtr)
      type(Node_tAVL), pointer :: nodePtr
      integer, intent(in) :: value
      allocate (nodePtr)
      nodePtr%Value = value
      nodePtr%Factor = 0
      nodePtr%Left => null()
      nodePtr%Right => null()
   end function NewNode

   subroutine newTree(self)
      class(Tree_t), intent(inout) :: self
      self%root => null()
   end subroutine newTree

   function rotationII(n, n1) result(result_node)
      type(Node_tAVL), pointer :: n, n1, result_node

      n%Left => n1%Right
      n1%Right => n
      if (n1%Factor == -1) then
         n%Factor = 0
         n1%Factor = 0
      else
         n%Factor = -1
         n1%Factor = 1
      end if
      result_node => n1
   end function rotationII

   function rotationDD(n, n1) result(result_node)
      type(Node_tAVL), pointer :: n, n1, result_node

      n%Right => n1%Left
      n1%Left => n
      if (n1%Factor == 1) then
         n%Factor = 0
         n1%Factor = 0
      else
         n%Factor = 1
         n1%Factor = -1
      end if
      result_node => n1
   end function rotationDD

   function rotationDI(n, n1) result(result_node)
      type(Node_tAVL), pointer :: n, n1, result_node, n2

      n2 => n1%Left
      n%Right => n2%Left
      n2%Left => n
      n1%Left => n2%Right
      n2%Right => n1
      if (n2%Factor == 1) then
         n%Factor = -1
      else
         n%Factor = 0
      end if
      if (n2%Factor == -1) then
         n1%Factor = 1
      else
         n1%Factor = 0
      end if
      n2%Factor = 0
      result_node => n2
   end function rotationDI

   function rotationID(n, n1) result(result_node)
      type(Node_tAVL), pointer :: n, n1, result_node, n2
      n2 => n1%Right
      n%Left => n2%Right
      n2%Right => n
      n1%Right => n2%Left
      n2%Left => n1
      if (n2%Factor == 1) then
         n1%Factor = -1
      else
         n1%Factor = 0
      end if
      if (n2%Factor == -1) then
         n%Factor = 1
      else
         n%Factor = 0
      end if
      n2%Factor = 0
      result_node => n2
   end function rotationID

   recursive function insert2(root, value, increase) result(result_node)
      type(Node_tAVL), pointer :: root, result_node, n1
      logical, intent(out) :: increase
      integer, intent(in) :: value

      if (.not. associated(root)) then
         allocate (result_node)
         root => NewNode(value)
         increase = .true.
      else if (value < root%Value) then
         root%Left => insert2(root%Left, value, increase)
         if (increase) then
            select case (root%Factor)
            case (RIGHT_HEAVY)
               root%Factor = 0
               increase = .false.
            case (BALANCED)
               root%Factor = -1
            case (LEFT_HEAVY)
               n1 => root%Left
               if (n1%Factor == -1) then
                  root => rotationII(root, n1)
               else
                  root => rotationID(root, n1)
               end if
               increase = .false.
            end select
         end if
      else if (value > root%Value) then
         root%Right => insert2(root%Right, value, increase)
         if (increase) then
            select case (root%Factor)
            case (RIGHT_HEAVY)
               n1 => root%Right
               if (n1%Factor == 1) then
                  root => rotationDD(root, n1)
               else
                  root => rotationDI(root, n1)
               end if
               increase = .false.
            case (BALANCED)
               root%Factor = 1
            case (LEFT_HEAVY)
               root%Factor = 0
               increase = .false.
            end select
         end if
      end if

      result_node => root
   end function insert2

   subroutine insert(tree, value)
      class(Tree_t), intent(inout) :: tree
      integer, intent(in) :: value
      logical :: increase

      increase = .false.
      tree%root => insert2(tree%root, value, increase)
   end subroutine insert

   subroutine GenerateGraph(this)
      class(Tree_t), intent(inout) :: this
      character(len=:), allocatable :: dotStructure, abbDotCode
      character(len=:), allocatable :: createNodes, linkNodes
  
      createNodes = ''
      linkNodes = ''
      abbDotCode = ''
  
      dotStructure = "digraph G{"//new_line('a')
      dotStructure = dotStructure//"node [shape=circle];"//new_line('a')
  
      if (associated(this%root)) then
          call RoamTree(this%root, createNodes, linkNodes, abbDotCode)
      end if
  
      dotStructure = dotStructure//trim(createNodes)//trim(linkNodes)//trim(abbDotCode)//"}"//new_line('a')
      call write_dot(dotStructure)
      print *, "Archivo actualizado existosamente."
  end subroutine GenerateGraph

  function empty_string() result(str)
   character(len=:), allocatable :: str
   str = ""
end function empty_string

recursive subroutine RoamTree(actual, createNodes, linkNodes, abbDotCode)
    type(Node_tAVL), pointer :: actual
    character(len=:), allocatable, intent(inout) :: createNodes, linkNodes, abbDotCode
    character(len=20) :: address, str_value
    character(len=:), allocatable :: abbCode
    character(len=:), allocatable :: abbCreateNodes, abbLinkNodes

    if (associated(actual)) then
        ! SE OBTIENE INFORMACION DEL NODO ACTUAL
        address = get_address_memory(actual)
        write (str_value, '(I0)') actual%Value
        createNodes = createNodes//'"'//trim(address)//'"'//'[label="'//trim(str_value)//&
        '", shape="rectangle", color="#0400ff",   style="diagonals"];'//new_line('a')

        ! Generar código dot para el ABB de este nodo AVL
        abbCreateNodes = ""
        abbLinkNodes = ""
        abbCode = generateABBDotCode(actual%NodoABB%root, abbCreateNodes, abbLinkNodes)
        abbDotCode = abbDotCode // abbCode

        ! Conectar el nodo AVL con el árbol ABB
        abbLinkNodes = ""
        call generateABBLinks(actual%NodoABB%root, address, abbLinkNodes)
        linkNodes = linkNodes // abbLinkNodes

        ! VIAJAMOS A LA SUBRAMA IZQ
        if (associated(actual%Left)) then
            linkNodes = linkNodes//'"'//trim(address)//'"'//" -> "
            address = get_address_memory(actual%Left)
            linkNodes = linkNodes//'"'//trim(address)//'" ' &
                        //'[label = "L"];'//new_line('a')
        end if
        ! VIAJAMOS A LA SUBRAMA DER
        if (associated(actual%Right)) then
            address = get_address_memory(actual)
            linkNodes = linkNodes//'"'//trim(address)//'"'//" -> "
            address = get_address_memory(actual%Right)
            linkNodes = linkNodes//'"'//trim(address)//'" ' &
                        //'[label = "R"];'//new_line('a')
        end if

        call RoamTree(actual%Left, createNodes, linkNodes, abbDotCode)
        call RoamTree(actual%Right, createNodes, linkNodes, abbDotCode)
    end if
end subroutine RoamTree

recursive subroutine generateABBLinks(root, avlAddress, linkNodes)
    type(Node_tABB), pointer :: root
    character(len=*), intent(in) :: avlAddress
    character(len=:), allocatable, intent(inout) :: linkNodes
    character(len=20) :: address

    if (associated(root)) then
        address = get_address_memoryABB(root)
        linkNodes = linkNodes//'"'//trim(avlAddress)//'"'//" -> "//'"'//trim(address)//'"' &
                    //' [style=dashed, arrowhead=none];'//new_line('a')
 
    end if
end subroutine generateABBLinks

function get_address_memoryABB(node) result(address)
    type(Node_tABB), pointer :: node
    character(len=20) :: address
    integer*8 :: i

    i = loc(node) ! get the address of x
    ! convert the address to string
    write (address, 10) i
10  format(I0)
end function get_address_memoryABB

   function get_address_memory(node) result(address)
      !class(matrix_t), intent(in) :: self
      type(Node_tAVL), pointer :: node
      character(len=20) :: address
      ! integer 8
      integer*8 :: i

      i = loc(node) ! get the address of x
      ! convert the address to string
      write (address, 10) i
10    format(I0)

   end function get_address_memory

   subroutine write_dot(code)
      character(len=*), intent(in) :: code
      open (10, file='graphAVL.dot', status='replace', action='write')
      write (10, '(A)') trim(code)
      close (10)
      ! Genera la imagen PNG
      call system("dot -Tpng graphAVL.dot -o GraficaAVL.png")
   end subroutine write_dot

   subroutine write_dot2(code)
      character(len=*), intent(in) :: code
      open(10, file='Grafica2AVL.dot', status='replace', action='write')
      write(10, '(A)') trim(code)
      close(10)
      ! Genera la imagen PNG
      call system("dot -Tpng Grafica2AVL.dot -o Grafica2AVL.png")
    end subroutine write_dot2


   subroutine GenerateGraph2(this)
      class(Tree_t), intent(inout) :: this
      character(len=:), allocatable :: dotStructure
      character(len=:), allocatable :: createNodes
      character(len=:), allocatable :: linkNodes
      createNodes = ''
      linkNodes = ''
  
  
      dotStructure = "digraph G{" // new_line('a')
      dotStructure = dotStructure // "node [shape=circle];" // new_line('a')
  
      if (associated(this%root)) then
          call RoamTree2(this%root, createNodes, linkNodes)
      end if
  
      dotStructure = dotStructure // trim(createNodes) // trim(linkNodes) // "}" // new_line('a')
      call write_dot2(dotStructure)
      print *, "Archivo actualizado existosamente."
  end subroutine GenerateGraph2
  
  recursive subroutine RoamTree2(actual, createNodes, linkNodes)
      type(Node_tAVL), pointer :: actual
      character(len=:), allocatable, intent(inout) :: createNodes
      character(len=:), allocatable, intent(inout) :: linkNodes
      character(len=20) :: address
      character(len=20) :: str_value
  
      if (associated(actual)) then
          ! SE OBTIENE INFORMACION DEL NODO ACTUAL
        address = get_address_memory(actual)
        write(str_value, '(I0)') actual%Value
        createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // trim(str_value) // '"];' // new_line('a')
        ! VIAJAMOS A LA SUBRAMA IZQ
        if (associated(actual%Left)) then
          linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
          address = get_address_memory(actual%Left)
          linkNodes = linkNodes // '"' // trim(address) // '" ' &
                    // '[label = "L"];' // new_line('a')
  
        end if
        ! VIAJAMOS A LA SUBRAMA DER
        if (associated(actual%Right)) then
          address = get_address_memory(actual)
          linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
          address = get_address_memory(actual%Right)
          linkNodes = linkNodes // '"' // trim(address) // '" ' &
                    // '[label = "R"];' // new_line('a')
        end if
  
        call RoamTree2(actual%Left, createNodes, linkNodes)
        call RoamTree2(actual%Right, createNodes, linkNodes)
      end if
  end subroutine RoamTree2

end module TreeAVL_M
