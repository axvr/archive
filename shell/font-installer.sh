#!/bin/sh

# A simple font installer for Unix-like OSs
#
# Author:  Alex Vear (axvr)
# Licence: Unlicence

# Usage examples:
# $ ./font-installer.sh inconsolata
# $ ./font-installer.sh "source code pro"
# $ ./font-installer.sh Source-Code-Pro roboto-mono
# $ ./font-installer.sh source_Code-pRO

requires() {
    for i in "$@"
    do
        if [ ! "$(command -v "$i")" ]
        then
            (>&2 printf "Error: '%s' is not installed\\n" "$i")
            exit
        fi
    done
}
requires "curl"

github_latest_version() {
    curl --silent "https://api.github.com/repos/$1/$2/releases/latest" |
    grep '"tag_name":' | sed 's/.*"\([^"]\+\)".*/\1/'
}

[ -z "$FONT_DIR" ] && FONT_DIR="$HOME/.fonts"
[ ! -d "$FONT_DIR" ] && mkdir -p "$FONT_DIR"

# TODO: Input (http://input.fontbureau.com/) - Ask permission to distribute?
# TODO: Iosevka (https://be5invis.github.io/Iosevka/)
# TODO: Liberation Mono (find home page)
# TODO: Liberation Sans (find home page)
# TODO: Terminus (http://terminus-font.sourceforge.net/)

font_list=$(cat <<EOF
Anonymous Pro       (https://www.marksimonson.com/fonts/view/anonymous-pro)
DejaVu Sans/Serif   (https://dejavu-fonts.github.io/)
Dijkstra            (http://lucacardelli.name/indexartifacts.html)
Fira Code           (https://github.com/tonsky/FiraCode)
Fira Mono           (https://mozilla.github.io/Fira/)
Fira Sans           (https://mozilla.github.io/Fira/)
Font Awesome        (https://fontawesome.com/)
GNU FreeFont        (https://www.gnu.org/software/freefont/)
Hack                (https://sourcefoundry.org/hack/)
Inconsolata         (http://www.levien.com/type/myfonts/inconsolata.html)
Mononoki            (https://madmalik.github.io/mononoki/)
Noto Sans           (https://fonts.google.com/specimen/Noto+Sans)
Noto Serif          (https://fonts.google.com/specimen/Noto+Serif)
Roboto Condensed    (https://fonts.google.com/specimen/Roboto+Condensed)
Roboto              (https://fonts.google.com/specimen/Roboto)
Roboto Mono         (https://fonts.google.com/specimen/Roboto+Mono)
Roboto Slab         (https://fonts.google.com/specimen/Roboto+Slab)
Share Tech Mono     (https://fonts.google.com/specimen/Share+Tech+Mono)
Source Code Pro     (https://adobe-fonts.github.io/source-code-pro/)
Spleen              (https://www.cambus.net/spleen-monospaced-bitmap-fonts/)
Tamsyn              (http://www.fial.com/~scott/tamsyn-font/)
EOF
)

# Simplify installing fonts from Google Fonts
google_fonts() {
    requires "unzip"
    url="https://fonts.google.com/download?family=$(printf "$1" | sed 's/\s/%20/g')"
    printf "Installing font: %s\\n" "$1"
    [ -d "${FONT_DIR:?}/${f}/" ] && rm -r "${FONT_DIR:?}/${f}/"
    curl -L "$url" -o "$FONT_DIR/$f/$f.zip" --create-dirs
    unzip "$FONT_DIR/$f/$f.zip" -d "$FONT_DIR/$f/"
}

[ ! -n "$1" ] && printf "%s\\n" "$font_list"
for f in "$@"
do 
    f="$(printf "$f" | tr '[:upper:]' '[:lower:]' | tr -s ' _-' '_')"
    case "$f" in
        roboto)             google_fonts "Roboto";;
        roboto_mono)        google_fonts "Roboto Mono";;
        roboto_slab)        google_fonts "Roboto Slab";;
        roboto_condensed)   google_fonts "Roboto Condensed";;
        source_code_pro)    google_fonts "Source Code Pro";;
        noto_sans)          google_fonts "Noto Sans";;
        noto_serif)         google_fonts "Noto Serif";;
        inconsolata)        google_fonts "Inconsolata";;
        fira_sans)          google_fonts "Fira Sans";;
        fira_mono)          google_fonts "Fira Mono";;
        share_tech_mono)    google_fonts "Share Tech Mono";;

        fira_code)
            requires "unzip"
            printf "Installing font: Fira Code\\n"
            [ -d "$FONT_DIR/fira-code/" ] && rm -r "$FONT_DIR/fira-code/"
            version="$(github_latest_version "tonsky" "FiraCode")"
            url="https://github.com/tonsky/FiraCode/releases/download/$version/FiraCode_$version.zip"
            curl -L "$url" -o "$FONT_DIR/fira-code/FiraCode_$version.zip" --create-dirs
            unzip "$FONT_DIR/fira-code/FiraCode_$version.zip" -d "$FONT_DIR/fira-code/"
            ;;

        hack)
            requires "tar" "gzip"
            printf "Installing font: Hack\\n"
            [ -d "$FONT_DIR/hack/" ] && rm -r "$FONT_DIR/hack/"
            version="$(github_latest_version "source-foundry" "Hack")"
            url="https://github.com/source-foundry/Hack/releases/download/$version/Hack-$version-ttf.tar.gz"
            curl -L "$url" -o "$FONT_DIR/hack/Hack-$version-ttf.tar.gz" --create-dirs
            tar -zxvf "$FONT_DIR/hack/Hack-$version-ttf.tar.gz" -C "$FONT_DIR/hack"
            ;;

        mononoki)
            requires "unzip"
            printf "Installing font: Mononoki\\n"
            [ -d "$FONT_DIR/mononoki/" ] && rm -r "$FONT_DIR/mononoki/"
            version="$(github_latest_version "madmalik" "mononoki")"
            url="https://github.com/madmalik/mononoki/releases/download/$version/mononoki.zip"
            curl -L "$url" -o "$FONT_DIR/mononoki/mononoki.zip" --create-dirs
            unzip "$FONT_DIR/mononoki/mononoki.zip" -d "$FONT_DIR/mononoki/"
            ;;

        dejavu)
            requires "tar" "bzip2"
            printf "Installing font: DejaVu\\n"
            [ -d "$FONT_DIR/dejavu-fonts-ttf-2.37/" ] && rm -r "$FONT_DIR/dejavu-fonts-ttf-2.37/"
            url="http://sourceforge.net/projects/dejavu/files/dejavu/2.37/dejavu-fonts-ttf-2.37.tar.bz2"
            curl -L "$url" -o "$FONT_DIR/dejavu-fonts-ttf-2.37/dejavu-fonts-ttf-2.37.tar.bz2" --create-dirs
            tar -jxvf "$FONT_DIR/dejavu-fonts-ttf-2.37/dejavu-fonts-ttf-2.37.tar.bz2" -C "$FONT_DIR/"
            ;;

        tamsyn)
            requires "tar" "gzip"
            printf "Installing font: Tamsyn\\n"
            [ -d "$FONT_DIR/tamsyn-font-1.11/" ] && rm -r "$FONT_DIR/tamsyn-font-1.11/"
            url="http://www.fial.com/~scott/tamsyn-font/download/tamsyn-font-1.11.tar.gz"
            curl -L "$url" -o "$FONT_DIR/tamsyn-font-1.11/tamsyn.tar.gz" --create-dirs
            tar -zxvf "$FONT_DIR/tamsyn-font-1.11/tamsyn.tar.gz" -C "$FONT_DIR/"
            ;;

        spleen)
            requires "tar" "gzip"
            printf "Installing font: Spleen\\n"
            [ -d "$FONT_DIR/spleen/" ] && rm -r "$FONT_DIR/spleen/"
            version="$(github_latest_version "fcambus" "spleen")"
            url="https://github.com/fcambus/spleen/releases/download/$version/spleen-$version.tar.gz"
            curl -L "$url" -o "$FONT_DIR/spleen/spleen-$version.tar.gz" --create-dirs
            tar -zxvf "$FONT_DIR/spleen/spleen-$version.tar.gz" -C "$FONT_DIR/spleen"
            ;;

        anonymous_pro)
            requires "unzip"
            printf "Installing font: Anonymous Pro\\n"
            [ -d "$FONT_DIR/anonymous-pro/" ] && rm -r "$FONT_DIR/anonymous-pro/"
            archive_name="AnonymousPro-1.002.zip"
            url="https://www.marksimonson.com/assets/content/fonts/$archive_name"
            curl -L "$url" -o "$FONT_DIR/anonymous-pro/$archive_name" --create-dirs
            unzip "$FONT_DIR/anonymous-pro/$archive_name" -d "$FONT_DIR/anonymous-pro"
            ;;

        font_awesome)
            requires "unzip"
            printf "Installing font: Font Awesome (free)\\n"
            [ -d "$FONT_DIR/font-awesome/" ] && rm -r "$FONT_DIR/font-awesome/"
            archive_name="fontawesome-free-5.2.0-desktop.zip"
            url="https://use.fontawesome.com/releases/v5.2.0/$archive_name"
            curl -L "$url" -o "$FONT_DIR/font-awesome/fontawesome-free-5.2.0-desktop.zip" --create-dirs
            unzip "$FONT_DIR/font-awesome/$archive_name" -d "$FONT_DIR/font-awesome/"
            mv "$FONT_DIR/font-awesome/fontawesome-free-5.2.0-desktop/"* "$FONT_DIR/font-awesome/"
            rmdir "$FONT_DIR/font-awesome/fontawesome-free-5.2.0-desktop/"
            ;;

        dijkstra)
            printf "Installing font: Dijkstra\\n"
            [ -d "$FONT_DIR/dijkstra/" ] && rm -r "$FONT_DIR/dijkstra/"
            url="http://lucacardelli.name/Artifacts/Fonts/Pc/dijkstra.ttf"
            curl -L "$url" -o "$FONT_DIR/dijkstra/dijkstra.ttf" --create-dirs
            ;;

        free_font|gnu_free_font|freefont|gnu_freefont)
            requires "unzip"
            printf "Installing font: GNU FreeFont\\n"
            [ -d "$FONT_DIR/freefont" ] && rm -r "$FONT_DIR/freefont"
            url="https://ftp.gnu.org/gnu/freefont/freefont-ttf-20120503.zip"
            curl -L "$url" -o "$FONT_DIR/freefont/freefont-ttf-20120503.zip" --create-dirs
            unzip "$FONT_DIR/freefont/freefont-ttf-20120503.zip" -d "$FONT_DIR/freefont"
            mv "$FONT_DIR/freefont/freefont-20120503/"* "$FONT_DIR/freefont/"
            rmdir "$FONT_DIR/freefont/freefont-20120503/"
            ;;

        *)  printf "Error: Invalid font '%s'\\n" "$f";;
    esac
done
