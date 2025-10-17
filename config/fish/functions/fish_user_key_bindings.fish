function fish_user_key_bindings
    if functions -q fzf_configure_bindings
        fzf_configure_bindings
    else if type -q fzf
        fzf --fish | source
    end

    if functions -q _atuin_search
        bind \ex _atuin_search
        bind -M insert \ex _atuin_search
    end
end
