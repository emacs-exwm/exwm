echo "The xinitrc file has been deprecated. We do not recommend using
xinitrc directly as is. Instead copy the relevant settings to your
xsession or xinitrc file and modify them as needed. The code from here
will be moved out of the source repository to the manual after the
next release. See https://github.com/emacs-exwm/exwm/issues/57."

# Disable access control for the current user.
xhost +SI:localuser:$USER

# Make Java applications aware this is a non-reparenting window manager.
export _JAVA_AWT_WM_NONREPARENTING=1

# Set default cursor.
xsetroot -cursor_name left_ptr

# Set keyboard repeat rate.
xset r rate 200 60

# Uncomment the following block to use the exwm-xim module.
#export XMODIFIERS=@im=exwm-xim
#export GTK_IM_MODULE=xim
#export QT_IM_MODULE=xim
#export CLUTTER_IM_MODULE=xim

# Finally start Emacs
exec emacs
