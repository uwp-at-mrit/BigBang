#include "pch.h"
#include "tongue.h"

static ResourceLoader^ tongue = nullptr;

String^ Win2D::UWRT::Tongue::Speak(String^ id) {
    if (tongue == nullptr) tongue = ResourceLoader::GetForCurrentView();
    return tongue->GetString(id);
}
