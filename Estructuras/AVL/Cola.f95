module Cola

   type queue_list
      integer :: size = 0
      type(node), pointer :: head => null()
      type(node), pointer :: tail => null()
      integer :: IDE
   contains
      procedure :: enqueue
      procedure :: dequeue
      procedure :: print
   end type queue_list

   type node
      integer :: id
      type(node), pointer :: next => null()
   end type node

contains

   subroutine enqueue(self, id)
      class(queue_list), intent(inout) :: self
      integer, intent(in) :: id
      type(node), pointer :: newNode
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
      class(queue_list), intent(inout) :: self
      type(node), pointer :: temp

      if (.not. associated(self%head)) then
         print *, "Empty queue"
      else
         temp => self%head
         self%head => self%head%next
         self%IDE = temp%id
         deallocate (temp)
         self%size = self%size - 1

         if (self%size == 0) then
            self%tail => null()
         end if
      end if
   end subroutine dequeue

   subroutine print(self)
      class(queue_list), intent(inout) :: self
      type(node), pointer :: current

      current => self%head

      if (.not. associated(current)) then
         print *, "Empty queue"
         return
      else
         do while (associated(current))
            print *, "| ID: ", current%id

            current => current%next
         end do
      end if
   end subroutine print

end module Cola
