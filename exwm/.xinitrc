# Disable access control for the current user
xhost +SI:localuser:$USER

# Make Java applications aware this is a non-reparenting window manager
export _JAVA_AWT_WM_NONREPARENTING=1

# Set keyboard repeat rate
xset r rate 200 60

# Set default cursor
xsetroot -cursor_name left_ptr

# Setup XIM
export CLUTTER_IM_MODULE=xim
export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim
export XMODIFIERS=@im=exwm-xim

# Finally start Emacs
exec emacs --load exwm --funcall exwm-enable
