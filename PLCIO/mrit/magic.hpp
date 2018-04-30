#pragma once

#include <cinttypes>

static const uint16 MR_TCP_DEFAULT_PORT      = 2000;
static const uint16 MRDB_FOR_ALL            = 98;

static const uint16 MRDB_REALTIME           = 2;
static const uint16 MRDB_FORAT              = 20;
static const uint16 MRDB_DIGITAL_INPUT      = 205;
static const uint16 MRDB_DIGITAL_INPUT_RAW  = 4;
static const uint16 MRDB_DIGITAL_OUTPUT_RAW = 6;
static const uint16 MRDB_ANALOG_INPUT       = 203;
static const uint16 MRDB_ANALOG_INPUT_RAW   = 3;
static const uint16 MRDB_ANALOG_OUTPUT      = 204;
static const uint16 MRDB_ANALOG_OUTPUT_RAW  = 5;

/* MRIT Protocol function codes */
static const uint8 MR_READ_SIGNAL            = 'A';
static const uint8 MR_WRITE_ANALOG_QUANTITY  = 'B';
static const uint8 MR_WRITE_DIGITAL_QUANTITY = 'C';
