#pragma once

namespace WarGrey::SCADA {
    class IPlanet;
	class IHeadUpPlanet;
    class IPlanetDecorator;

	class ISprite;
    class IGraphlet;
	class IKeyboard;

	struct UniverseFigure;

    private enum class GraphletAnchor { LT, CT, RT, LC, CC, RC, LB, CB, RB };
	private enum class GraphletGesture { ZoomIn, ZoomOut, TranslateX, TranslateY, _ };
	private enum class ScreenKeyboard { Numpad, Affinepad, Bucketpad };
}
