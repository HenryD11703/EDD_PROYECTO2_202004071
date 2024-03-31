module BTree
    implicit none
    

    ! Order 5
    integer, parameter :: MAXI = 4, MINI = 2 

    type nodeptr
        type (BTreeNode), pointer :: ptr => null()
    end type nodeptr

    type BTreeNode
        integer(kind=8) :: val(0:MAXI+1)  ! Cambia a INTEGER(kind=8)
        integer :: num = 0
        type(nodeptr) :: link(0:MAXI+1)
    end type BTreeNode

    type(BTreeNode), pointer :: root => null()

    contains

    subroutine insertB(val)
        integer(kind=8), intent(in) :: val  ! Cambia a INTEGER(kind=8)
        integer(kind=8) :: i  ! Cambia a INTEGER(kind=8)
        type(BTreeNode), pointer :: child
        allocate(child)
        if (setValue(val, i, root, child)) then
            root => createNode(i, child)
        end if
    end subroutine insertB

    recursive function setValue(val, pval, node, child) result(res)
        integer(kind=8), intent(in) :: val  ! Cambia a INTEGER(kind=8)
        integer(kind=8), intent(inout) :: pval  ! Cambia a INTEGER(kind=8)
        type(BTreeNode), pointer, intent(inout) :: node
        type(BTreeNode), pointer, intent(inout) :: child
        type(BTreeNode), pointer :: newnode        
        integer(kind=8) :: pos
        logical :: res
        allocate(newnode)
        if (.not. associated(node)) then            
            pval = val
            child => null()
            res = .true.
            return
        end if
        if (val < node%val(1)) then
            pos = 0
        else
            pos = node%num
            do while (val < node%val(pos) .and. pos > 1) 
            pos = pos - 1
            end do
            if (val == node%val(pos)) then
                print *, "Duplicates are not permitted"
                res = .false.
                return
            end if
        end if
        if (setValue(val, pval, node%link(pos)%ptr, child)) then
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

    subroutine insertNode(val, pos, node, child)
        integer(kind=8), intent(in) :: val, pos  ! Cambia a INTEGER(kind=8)
        type(BTreeNode), pointer, intent(inout) :: node
        type(BTreeNode), pointer, intent(in) :: child
        integer :: j
        j = node%num
        do while (j > pos)
            node%val(j + 1) = node%val(j)
            node%link(j + 1)%ptr => node%link(j)%ptr
            j = j - 1
        end do
        node%val(j + 1) = val
        node%link(j + 1)%ptr => child
        node%num = node%num + 1
    end subroutine insertNode

    subroutine splitNode(val, pval, pos, node, child, newnode)
        integer(kind=8), intent(in) :: val, pos  ! Cambia a INTEGER(kind=8)
        integer(kind=8), intent(inout) :: pval  ! Cambia a INTEGER(kind=8)
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
            call insertNode(val, pos, node, child)
        else
            call insertNode(val, pos - median, newnode, child)
        end if        
        pval = node%val(node%num)        
        newnode%link(0)%ptr => node%link(node%num)%ptr
        node%num = node%num - 1
    end subroutine splitNode

    function createNode(val, child) result(newNode)
        integer(kind=8), intent(in) :: val  ! Cambia a INTEGER(kind=8)
        type(BTreeNode), pointer, intent(in) :: child
        type(BTreeNode), pointer :: newNode
        integer :: i
        allocate(newNode)
        newNode%val(1) = val
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
                write (*,'(1I20)', advance='no') myNode%val(i+1)
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
                write(unit, '(1I20)', advance='no') myNode%val(i+1)
                write(unit, '(A)', advance='no') '|<f' // trim(adjustl(itoaC(i+1))) // '> '
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