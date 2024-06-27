function fish_user_key_bindings
  fzf --fish | source
  bind \ex _atuin_search
  bind -M insert \ex _atuin_search
end
