module List_of_lists_m
  implicit none
  private

  type :: string_node
      integer :: value
      type(string_node), pointer :: next => null()
  end type string_node

  type :: node
      character(:), allocatable :: index
      type(node), pointer :: next => null()
      type(node), pointer :: prev => null()
      type(string_node), pointer :: head => null()
  contains
      procedure :: append
      procedure :: print
  end type node
  
  type, public :: List_of_lists
      type(node), pointer :: head => null()
      type(node), pointer :: tail => null()
  
  contains
      procedure :: insert
      procedure :: printList
      procedure :: graphdot
  end type List_of_lists

contains
!index = 1
!0 -> ...  <- aux
!1 ->
!4 -> ...  <- aux%next
  subroutine insert(self, index, value)
      class(List_of_lists), intent(inout) :: self
      integer, intent(in) :: value
      character(:), allocatable :: index
       

      type(node), pointer :: aux
      type(node), pointer :: new
      allocate(new)

      if(.not. associated(self%head)) then
          new%index = index
          self%head => new
          self%tail => new
          call new%append(value)
      else
          if(index < self%head%index) then
              self%head%prev => new
              new%next => self%head
              self%head => new

              new%index = index
              call new%append(value)
          else
              aux => self%head
              do while (associated(aux%next))
                  if(index < aux%next%index) then
                      if(index == aux%index) then
                          call aux%append(value)
                      else
                          new%next => aux%next
                          new%prev => aux
                          aux%next%prev => new
                          aux%next => new

                          new%index = index
                          call new%append(value)
                      end if
                      return
                  end if
                  aux => aux%next
              end do

              if(index == aux%index) then
                  call aux%append(value)
              else
                  self%tail%next => new
                  new%prev => self%tail
                  self%tail => new

                  new%index = index
                  call new%append(value)
              end if
          end if
      end if
  end subroutine insert

  subroutine printList(self)
      class(List_of_lists), intent(in) :: self
      type(node), pointer :: aux

      aux => self%head

      do while(associated(aux))
          print *, 'INDICE: ', aux%index
          call aux%print()
          print *, ""
          aux => aux%next
      end do
  end subroutine printList


  ! Función que elimina los espacios de una cadena para usar en el archivo .dot
  function sin_espacios(cadena) result(cadena_sin_espacios)
    implicit none
    character(len=*) :: cadena
    character(len=len(cadena)) :: cadena_sin_espacios
    integer :: i, j, k
    k = 0
    do i = 1, len(cadena)
      if (cadena(i:i) /= ' ') then
        k = k + 1
        cadena_sin_espacios(k:k) = cadena(i:i)
      end if
    end do
    cadena_sin_espacios = cadena_sin_espacios(1:k)
    !print *, cadena_sin_espacios
  end function sin_espacios
    
  subroutine graphDot(self, filename)
    class(List_of_lists), intent(inout) :: self
    type(string_node), pointer :: aux
      character(len=*), intent(in) :: filename
      type(node), pointer :: currentLL, firstLL
      character(:), allocatable :: id, firstId

      currentLL => self%head
        firstLL => self%head
        firstId = sin_espacios(firstLL%index)
      open(10, file=filename, status='replace')

      write(10, '(a)') 'digraph G {'
      write(10, '(a)') 'rankdir=LR;'
      write(10, '(a)') 'node [shape=record];'

      if ( .not. associated(currentLL) ) then
        write(10, '(a)') 'EmptyList [label="empty"];'        
      else 
        do
          id = sin_espacios(currentLL%index)
          write(10,'(a,a,a,a,a)') 'Node', trim(id), ' [label="', trim(id), '"];'
          if (associated(currentLL%prev)) then
            write(10,'(a,a,a,a,a)') 'Node', sin_espacios(currentLL%prev%index), '->Node', trim(id), ';'
        end if
        if (associated(currentLL%next)) then
            write(10,'(a,a,a,a,a)') 'Node', sin_espacios(currentLL%next%index), '->Node',trim(id), ';'
        end if
        
        !Ahora graficar la lista que contiene cada nodo
        aux => currentLL%head
        if (.not. associated(aux)) then
            write(10, '(a,a,a,a,a)') 'Node', trim(id), ' [label="', trim(id), '"];'
        else
            write(10, '(a,a,a,a,a)') 'Node', trim(id), ' [label="', trim(id), '|'
            do while (associated(aux))
                write(10, '(i0)') aux%value
                if (associated(aux%next)) write(10, '(a,a,a,a,a)') '|'
                aux => aux%next
            end do
            write(10, '(a,a,a,a,a)') '"];'
        end if

        !Hacer una conexion entre el primer nodo de la lista y el ultimo
        
 
          currentLL => currentLL%next
            if (id == sin_espacios(self%tail%index)) then
        write(10,'(a,a,a,a,a)') 'Node', trim(id), '->Node', trim(firstId), ';'
        write(10,'(a,a,a,a,a)') 'Node', trim(firstId), '->Node', trim(id), ';'
        exit
        end if
        end do
      end if
      
      write(10, '(a)') '}'
        close(10)

      
      print *, "Generando archivo .dot"

  end subroutine graphDot

  subroutine append(self, value)
      class(node), intent(inout) :: self
      integer, intent(in) :: value

      type(string_node), pointer :: aux
      type(string_node), pointer :: new
      allocate(new)
      new%value = value

      if(.not. associated(self%head)) then
          self%head => new
      else
          aux => self%head
          do while(associated(aux%next))
              aux => aux%next
          end do
          aux%next => new
      end if
  end subroutine

  subroutine print(self)
      class(node), intent(in) :: self
      type(string_node), pointer :: aux
      aux => self%head

      do while(associated(aux))
          print *, aux%value
          aux => aux%next
      end do
  end subroutine print

end module List_of_lists_m