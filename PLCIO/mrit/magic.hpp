#pragma once

#include <cinttypes>

static const uint16 MR_TCP_DEFAULT_PORT      = 2100;
static const uint8 MR_PROTOCOL_HEADER_LENGTH = (uint8)(1 + 1 + 2 + 2 + 2 + 2);

static const uint8 MR_PROTOCOL_HEAD = 0x24;   // $
static const uint16 MR_PROTOCOL_END = 0x0D0A; // Carriage Return + Line Feed

/* MRIT Protocol function codes */
static const uint8 MR_READ_SIGNAL            = 'A';
static const uint8 MR_WRITE_ANALOG_QUANTITY  = 'B';
static const uint8 MR_WRITE_DIGITAL_QUANTITY = 'C';
