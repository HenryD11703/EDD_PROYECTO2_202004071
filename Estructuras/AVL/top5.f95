module LinkedListModule
    implicit none

    type :: Node
        integer :: NumeroAVL
        integer :: NumeroABBs
        type(Node), pointer :: next => null()
    end type Node

    type :: LinkedList
        type(Node), pointer :: head => null()
    end type LinkedList

    contains

    subroutine AddNode(list, NumeroAVL, NumeroABBs)
        type(LinkedList), intent(inout) :: list
        integer, intent(in) :: NumeroAVL, NumeroABBs
        type(Node), pointer :: newNode

        allocate(newNode)
        newNode%NumeroAVL = NumeroAVL
        newNode%NumeroABBs = NumeroABBs
        newNode%next => null()

        if (associated(list%head)) then
            newNode%next => list%head
        end if

        list%head => newNode
    end subroutine AddNode

    subroutine SortList(list)
        type(LinkedList), intent(inout) :: list
        type(Node), pointer :: currentNode, nextNode
        integer :: tempNumeroAVL, tempNumeroABBs
        logical :: swapped
    
        if (.not. associated(list%head)) return
    
        do
            swapped = .false.
            currentNode => list%head
    
            do while (associated(currentNode%next))
                nextNode => currentNode%next
    
                if (currentNode%NumeroAVL > nextNode%NumeroAVL) then
                    tempNumeroAVL = currentNode%NumeroAVL
                    tempNumeroABBs = currentNode%NumeroABBs
    
                    currentNode%NumeroAVL = nextNode%NumeroAVL
                    currentNode%NumeroABBs = nextNode%NumeroABBs
    
                    nextNode%NumeroAVL = tempNumeroAVL
                    nextNode%NumeroABBs = tempNumeroABBs
    
                    swapped = .true.
                end if
    
                currentNode => currentNode%next
            end do
    
            if (.not. swapped) exit
        end do
    end subroutine SortList

    subroutine PrintList(list)
        type(LinkedList), intent(in) :: list
        type(Node), pointer :: currentNode
        integer :: count
    
        count = 0
        currentNode => list%head
    
        print *, 'TOP 5:'
        do while (associated(currentNode) .and. count < 5)
            print *, count+1, '  NumeroAVL:', currentNode%NumeroAVL,&
             'NumeroABBs:', currentNode%NumeroABBs
            currentNode => currentNode%next
            count = count + 1
        end do
    end subroutine PrintList
 

end module LinkedListModule

 
