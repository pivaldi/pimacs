#!/usr/bin/php
<?php

/**
  PHP CS Fixer Configuration.
  @author   Philippe IVALDI
  @license  GNU General Public License V3
  @category Configuration
  @package  N.A
  @link     https://github.com/pivaldi/pimacs
 */

error_reporting(E_ALL | E_STRICT);

$finder = \PhpCsFixer\Finder::create();

$config = new \PhpCsFixer\Config();
$config->setRules(
    [
        '@PSR2' => true,
        'array_syntax' => ['syntax' => 'short'],
        'array_indentation' => true,
        'method_argument_space' => ['on_multiline' => 'ensure_fully_multiline'],
        'no_trailing_whitespace' => true,
        'multiline_whitespace_before_semicolons' => true,
        'declare_equal_normalize' => [
            'space' => 'single',
        ],
        'binary_operator_spaces' => [
            'default' => 'single_space'
        ],
        'concat_space' => ['spacing' => 'one'],
    ]
);

return $config->setFinder($finder);
