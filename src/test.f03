program main
    ! Import module
    use multiarm_bandit_module
    use greedy_y_agent_module
    use logger_module

    ! Declare variable
    type(multiarm_bandit), dimension(10) :: bandit
    type(greedy_y_agent) :: GY_agent
    type(logger) :: l1
    integer, dimension(2) :: reward = [0,1]
    integer, dimension(2,10) :: weight = reshape([91, 9, 61, 39, 1, 99, 68, 32, 84, 16, 65, 35, 11, 89, 99, 1, 23, 77, 98, 2], [2,10])
    integer :: i

    ! Generate 10 multi-arm bandit instance, then assign with predefined reward and reward's weight
    do i = 1,10
        CALL set_reward(bandit(i), reward, weight(:, i))
    end do

    ! Initialized agent
    CALL init_y_agent(GY_agent, bandit, 0.7)

    print *, iter_y_agent(GY_agent)

    CALL init_log(l1, "./test.log")

    CALL write_log(l1, "This is a test")
    CALL write_log(l1, "Now, this is test2")

    CALL end_log(l1)





end program main