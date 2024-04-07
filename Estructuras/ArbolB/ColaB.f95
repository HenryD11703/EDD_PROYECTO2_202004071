module ColaB

   type queue_listB
      integer :: size = 0
      type(nodeB), pointer :: head => null()
      type(nodeB), pointer :: tail => null()
      integer(kind=8) :: id
   contains
      procedure :: enqueue
      procedure :: dequeue
 
   end type queue_listB

   type nodeB
      integer(kind=8) :: id
      type(nodeB), pointer :: next => null()
   end type nodeB

contains

   subroutine enqueue(self, id)
      class(queue_listB), intent(inout) :: self
      integer(kind=8), intent(inout) :: id
      type(nodeB), pointer :: newNode
      allocate (newNode)

      newNode%id = id
      newNode%next => null()

      if (.not. associated(self%head)) then
         self%head => newNode
         self%tail => newNode
      else
         self%tail%next => newNode
         self%tail => newNode
      end if

      self%size = self%size + 1

   end subroutine enqueue

   subroutine dequeue(self)
      class(queue_listB), intent(inout) :: self
      type(nodeB), pointer :: temp

      if (.not. associated(self%head)) then
         print *, "Empty queue"
      else
         temp => self%head
         self%head => self%head%next
         self%id = temp%id
         deallocate (temp)
         self%size = self%size - 1

         if (self%size == 0) then
            self%tail => null()
         end if
      end if
   end subroutine dequeue

   

end module ColaB
