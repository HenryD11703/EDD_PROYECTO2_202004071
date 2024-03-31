module BTree
    implicit none

    type UsuarioB
        integer(kind=8) :: id  ! kind = 8 para guardar el numero de DPI
        character(len=50) :: nombre
        character(len=50) :: contrasena
    end type UsuarioB

    ! Order 5
    integer, parameter :: MAXI = 4, MINI = 2 

    type nodeptr
        type (BTreeNode), pointer :: ptr => null()
    end type nodeptr

    type BTreeNode
        type(UsuarioB) :: val(0:MAXI+1)  
        integer :: num = 0
        type(nodeptr) :: link(0:MAXI+1)
    end type BTreeNode

    type(BTreeNode), pointer :: root => null()

    contains

   !Caso 1: El nodo es una hoja y tiene más de t claves. En este caso, simplemente eliminamos la clave del nodo.

    !Caso 2: El nodo es una hoja y tiene exactamente t claves. Aquí, la eliminación de una clave puede hacer que el nodo tenga menos de t claves, lo cual es una violación de las propiedades del árbol B. Para manejar esto, podemos:

    !a. Caso 2a: Si el hermano adyacente tiene más de t claves, entonces realizamos una rotación: movemos una clave del padre al nodo, movemos la clave del hermano al padre y movemos el hijo del hermano al nodo.

    !b. Caso 2b: Si el hermano adyacente tiene exactamente t claves, entonces fusionamos el nodo con su hermano. La clave del padre que está entre estos dos nodos se mueve hacia abajo y se fusiona con ellos.

    !Caso 3: La clave está en un nodo interno N. En este caso, necesitamos reemplazar la clave con su predecesor o sucesor que estará en una hoja. Luego, eliminamos esa clave de la hoja. Esto puede resultar en un caso 2 en la hoja.

    !a. Caso 3a: Si el hijo que precede a K en N tiene al menos t claves, entonces encontramos el predecesor de K en el subárbol de ese hijo. Reemplazamos K por el predecesor y recursivamente eliminamos el predecesor en el subárbol del hijo.

    !b. Caso 3b: Si el hijo que sigue a K en N tiene al menos t claves, entonces encontramos el sucesor de K en el subárbol de ese hijo. Reemplazamos K por el sucesor y recursivamente eliminamos el sucesor en el subárbol del hijo.

!   c. Caso 3c: Si ninguno de los hijos adyacentes a K en N tiene más de t-1 claves, entonces combinamos K y todos los descendientes de K que están en los dos hijos en un solo nodo.
    

 
    
    recursive function checkUser(id, password) result(isValid)
    integer(kind=8), intent(in) :: id
    character(len=50), intent(in) :: password
    logical :: isValid
    type(BTreeNode), pointer :: currentNode
    currentNode => root
    isValid = .false.
    call searchUser(currentNode, id, password, isValid)
end function checkUser

recursive subroutine searchUser(node, id, password, isValid)
    type(BTreeNode), pointer, intent(in) :: node
    integer(kind=8), intent(in) :: id
    character(len=50), intent(in) :: password
    logical, intent(out) :: isValid
    integer :: i

    if (associated(node)) then
        i = 0
        do while (i < node%num)
            if (node%val(i+1)%id == id .and. trim(node%val(i+1)%contrasena) == trim(password)) then
                isValid = .true.
                return
            end if
            call searchUser(node%link(i)%ptr, id, password, isValid)
            if (isValid) then
                return
            end if
            i = i + 1
        end do
        call searchUser(node%link(i)%ptr, id, password, isValid)
    end if
end subroutine searchUser

    subroutine insertB(usuario)
        type(UsuarioB), intent(in) :: usuario
        type(UsuarioB) :: i
        type(BTreeNode), pointer :: child
        allocate(child)
        if (setValue(usuario, i, root, child)) then
            root => createNode(i, child)
        end if
    end subroutine insertB

    recursive function setValue(user, pval, node, child) result(res)
        type(UsuarioB), intent(in) :: user
        type(UsuarioB), intent(inout) :: pval
        type(BTreeNode), pointer, intent(inout) :: node
        type(BTreeNode), pointer, intent(inout) :: child
        type(BTreeNode), pointer :: newnode        
        integer(kind=8) :: pos
        logical :: res
        allocate(newnode)
        if (.not. associated(node)) then            
            pval = user
            child => null()
            res = .true.
            return
        end if
        if (user%id < node%val(1)%id) then
            pos = 0
        else
            pos = node%num
            do while (user%id < node%val(pos)%id .and. pos > 1) 
            pos = pos - 1
            end do
            if (user%id == node%val(pos)%id) then
                print *, "Duplicates are not permitted"
                res = .false.
                return
            end if
        end if
        if (setValue(user, pval, node%link(pos)%ptr, child)) then
            if (node%num < MAXI) then
                call insertNode(pval, pos, node, child)
            else
                call splitNode(pval, pval, pos, node, child, newnode)
                child => newnode
                res = .true.
            return
        end if
    end if
    res = .false.
end function setValue

    subroutine insertNode(user, pos, node, child)
        type(UsuarioB), intent(in) :: user
        integer(kind=8), intent(in) :: pos  ! Cambia a INTEGER(kind=8)
        type(BTreeNode), pointer, intent(inout) :: node
        type(BTreeNode), pointer, intent(in) :: child
        integer :: j
        j = node%num
        do while (j > pos)
            node%val(j + 1) = node%val(j)
            node%link(j + 1)%ptr => node%link(j)%ptr
            j = j - 1
        end do
        node%val(j + 1) = user
        node%link(j + 1)%ptr => child
        node%num = node%num + 1
    end subroutine insertNode

    subroutine splitNode(user, pval, pos, node, child, newnode)
        type(UsuarioB), intent(in) :: user
        integer(kind=8), intent(in) :: pos  
        type(UsuarioB), intent(inout) :: pval  
        type(BTreeNode), pointer, intent(inout) :: node,  newnode
        type(BTreeNode), pointer, intent(in) ::  child
        integer :: median, i, j
        if (pos > MINI) then
            median = MINI + 1
        else
            median = MINI
        end if
        if (.not. associated(newnode)) then
            allocate(newnode)
        do i = 0, MAXI
            newnode%link(i)%ptr => null()
        enddo
        end if
        j = median + 1
        do while (j <= MAXI)
            newnode%val(j - median) = node%val(j)
            newnode%link(j - median)%ptr => node%link(j)%ptr
            j = j + 1
        end do
        node%num = median
        newnode%num = MAXI - median
        if (pos <= MINI) then
            call insertNode(user, pos, node, child)
        else
            call insertNode(user, pos - median, newnode, child)
        end if        
        pval = node%val(node%num)        
        newnode%link(0)%ptr => node%link(node%num)%ptr
        node%num = node%num - 1
    end subroutine splitNode

    function createNode(user, child) result(newNode)
        type(UsuarioB), intent(in) :: user
        type(BTreeNode), pointer, intent(in) :: child
        type(BTreeNode), pointer :: newNode
        integer :: i
        allocate(newNode)
        newNode%val(1) = user
        newNode%num = 1
        newNode%link(0)%ptr => root
        newNode%link(1)%ptr => child
        do i = 2, MAXI
            newNode%link(i)%ptr => null()
        end do
    end function createNode

    recursive subroutine traversal(myNode)
        type(BTreeNode), pointer, intent(in) :: myNode
        integer :: i
        if (associated(myNode)) then
            write (*, '(A)', advance='no') ' [ '
            i = 0
            do while (i < myNode%num)
                write (*,'(1I20)', advance='no') myNode%val(i+1)%id
                i = i + 1
            end do
            do i = 0, myNode%num
                call traversal(myNode%link(i)%ptr)    
            end do
            write (*, '(A)', advance='no') ' ] '
        end if
    end subroutine traversal

    recursive subroutine generateGraphviz(myNode, unit)
    type(BTreeNode), pointer, intent(in) :: myNode
    integer, intent(in) :: unit
    integer :: i

    if (associated(myNode)) then
        write(unit, '(A)', advance='no') 'node' // trim(adjustl(itoa(loc(myNode)))) // ' [label = "<f0> '
        i = 0
        do while (i < myNode%num)
            if (i /= 0) then
                write(unit, '(A)', advance='no') '|'
            end if
            write(unit, '(1I20)', advance='no') myNode%val(i+1)%id
            write(unit, '(A)', advance='no') '\n' // trim(myNode%val(i+1)%nombre) // '|<f' // trim(adjustl(itoaC(i+1))) // '> '
            i = i + 1
        end do
        write(unit, '(A)') '"];'
        do i = 0, myNode%num
            if (associated(myNode%link(i)%ptr)) then
                write(unit, '(A)') '"node' // trim(adjustl(itoa(loc(myNode)))) // '":f' &
                // trim(adjustl(itoaC(i))) // ' -> "node' &
                // trim(adjustl(itoa(loc(myNode%link(i)%ptr)))) // '";'
                call generateGraphviz(myNode%link(i)%ptr, unit)
            end if
        end do
    end if
end subroutine generateGraphviz

    function itoa(i) result(str)
        integer(kind=8), intent(in) :: i
        character(len=32) :: str
        write(str, '(I0)') i
        str = adjustl(str)
    end function itoa

    function itoaC(i) result(str)
        integer , intent(in) :: i
        character(len=32) :: str
        write(str, '(I0)') i
    end function itoaC

    subroutine writeGraphviz(root, filename)
        type(BTreeNode), pointer, intent(in) :: root
        character(len=*), intent(in) :: filename
        integer :: unit

        open(newunit=unit, file=filename, status='replace', action='write')
        write(unit, '(A)') 'digraph g {'
        write(unit, '(A)') 'node [shape = record,height=.1];'
        call generateGraphviz(root, unit)
        write(unit, '(A)') '}'
        close(unit)
    end subroutine writeGraphviz
end module BTree