#pragma once

#include <cinttypes>

static const uint16 MR_TCP_DEFAULT_PORT      = 2000;

static const uint16 MRDB_FOR_ALL             = 98;

/* MRIT Protocol function codes */
static const uint8 MR_READ_SIGNAL            = 'A';
static const uint8 MR_WRITE_ANALOG_QUANTITY  = 'B';
static const uint8 MR_WRITE_DIGITAL_QUANTITY = 'C';
