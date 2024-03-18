module Header_m
 
    private


    type, public :: matrix_node
        integer ::  row, col
        character(:), allocatable :: value
        type(matrix_node), pointer :: left => null()
        type(matrix_node), pointer :: right => null()
        type(matrix_node), pointer :: up => null()
        type(matrix_node), pointer :: down => null()
    end type matrix_node

    type, public:: Header_node
        integer :: position = 0 
        type(header_node), pointer :: next => null()
        type(header_node), pointer :: prev => null()
        type(matrix_node), pointer :: access => null()
        contains
            procedure :: init_Header_node
    end type Header_node

    type, public :: Header_t
        type(Header_node), pointer :: first => null()
        type(Header_node), pointer :: last => null()
        integer :: size = 0
        contains
            procedure, public :: init_Header_t
            procedure :: add
            procedure :: print
    end type Header_t

    contains

    subroutine init_Header_t(self)
        class(header_t), intent(inout) :: self
        self%size = 0
        self%first => null()
        self%last => null()
    end subroutine init_Header_t

    subroutine init_Header_node(self)
        class(Header_node), intent(inout) :: self
        self%position = 0
    end subroutine init_Header_node

    function add(self,pos)
        class(Header_t), intent(inout) :: self
        integer, intent(in) :: pos
        type(Header_node), pointer :: new_node
        type(Header_node), pointer :: current
        type(Header_node), pointer :: add ! return value
        allocate(new_node)
        new_node%position = pos
        if(self%size == 0) then
            self%first => new_node
            self%last => new_node
            self%size = self%size + 1
            add => new_node ! return the new node
        else
            if (pos == self%first%position) then
                add => self%first
                return
            else if(pos == self%last%position) then
                add => self%last
                return                
            end if
            ! Insert at the beginning
        if(pos < self%first%position) then
            new_node%next => self%first
            self%first%prev => new_node
            self%first => new_node
            self%size = self%size + 1
            add => new_node ! return the new node
        else if(pos > self%last%position) then
            new_node%prev => self%last
            self%last%next => new_node
            self%last => new_node
            self%size = self%size + 1
            add => new_node ! return the new node
        else
            current => self%first%next
            do while(associated(current))
                if(current%position == pos) then
                    deallocate(new_node)
                    add => current
                    exit
                else if(current%position > pos) then
                    new_node%next => current
                    new_node%prev => current%prev
                    current%prev%next => new_node
                    current%prev => new_node
                    self%size = self%size + 1
                    add => new_node ! return the new node
                    exit
                end if
                    current => current%next
                end do
            end if
        end if
    end function add

    subroutine print(self)
        class(Header_t), intent(inout) :: self
        type(header_node), pointer :: current
        current => self%first
  
        do while(associated(current))
            print *, current%position, ","
            current => current%next
        end do
    end subroutine

end module Header_m
            