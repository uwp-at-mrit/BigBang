#pragma once

#define read_only_property(T, name)  property T name { T get(); }
#define write_only_property(T, name)  property T name { void set(T v); }
#define read_write_property(T, name) property T name { T get(); void set(T v); }
