#pragma once

private value struct TextExtent {
    float width;
    float height;
    float tspace;
    float rspace;
    float bspace;
    float lspace;
};

Microsoft::Graphics::Canvas::CanvasDrawingSession^ make_shared_drawing_session();
Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ make_text_format(float size = 12.0F);

TextExtent get_text_extent(
    Platform::String^ message,
    Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font);

TextExtent get_text_extent(
    Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
    Platform::String^ message,
    Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font);
