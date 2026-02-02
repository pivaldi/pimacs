#!/usr/bin/env sh

EXPECTED_CHECKSUM="$(php -r 'copy("https://composer.github.io/installer.sig", "php://stdout");')"
php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"
ACTUAL_CHECKSUM="$(php -r "echo hash_file('sha384', 'composer-setup.php');")"

if [ "$EXPECTED_CHECKSUM" != "$ACTUAL_CHECKSUM" ]; then
    >&2 echo 'ERROR: Invalid installer checksum'
    rm composer-setup.php
    exit 1
fi

php composer-setup.php --quiet
rm composer-setup.php

COMPOSER_I_DIR=~/.local/bin/
COMPOSER="${COMPOSER_I_DIR}composer"

[ -e "$COMPOSER_I_DIR" ] || mkdir -p "$COMPOSER_I_DIR"
mv ./composer.phar "$COMPOSER"

COMPOSER_GLOGAL_I_DIR="$(${COMPOSER_I_DIR}composer global config bin-dir --absolute)"

echo "COMPOSER_GLOGAL_I_DIR=$COMPOSER_GLOGAL_I_DIR"

$COMPOSER global require symfony/filesystem=* techlivezheng/phpctags=* \
    squizlabs/php_codesniffer=* \
    vimeo/psalm=* \
    phpstan/phpstan=* \
    friendsofphp/php-cs-fixer=* \
    phpunit/phpunit=* \
    phpactor/language-server-phpstan-extension=* -W --no-progress

curl -Lo phpactor.phar https://github.com/phpactor/phpactor/releases/latest/download/phpactor.phar &&
    chmod +x phpactor.phar &&
    mv phpactor.phar "${COMPOSER_GLOGAL_I_DIR}/phpactor"
