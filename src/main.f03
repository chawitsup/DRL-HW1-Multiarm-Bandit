program main
    use multiarm_bandit_module

    type(multiarm_bandit) :: foo
    call init(foo)
    print *, foo%weight

end program main