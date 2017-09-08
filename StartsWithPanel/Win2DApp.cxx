#include "workspace.hxx"

using namespace Windows::Foundation;
using namespace Windows::ApplicationModel;
using namespace Windows::ApplicationModel::Activation;
using namespace Windows::ApplicationModel::Core;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::ViewManagement;

namespace WarGrey::Win2DDemo {
    private ref class Win2DApp sealed : public Application {
    protected:
        void Win2DAppMain(ApplicationView^ self, WorkSpace^ workspace) {
            this->Suspending += ref new SuspendingEventHandler(this, &Win2DApp::OnSuspending);
            self->VisibleBoundsChanged += ref new TypedEventHandler<ApplicationView^, Object^>(this, &Win2DApp::DoResize);
            
            ApplicationView::PreferredLaunchWindowingMode = ApplicationViewWindowingMode::PreferredLaunchViewSize;
            ApplicationView::PreferredLaunchViewSize = get_screen_size();

            // WARNING: Force Using the default TitleBar if a custom one was set once.
            CoreApplication::GetCurrentView()->TitleBar->ExtendViewIntoTitleBar = false;
            self->Title = workspace->ToString();
        }

        virtual void OnLaunched(LaunchActivatedEventArgs^ e) override {
            auto self = ApplicationView::GetForCurrentView();
            auto workspace = dynamic_cast<WorkSpace^>(Window::Current->Content);

            if (workspace != nullptr) {
                if (e->PrelaunchActivated == false) {
                    workspace->initialize_component(adjusted_workspace_size(self->VisibleBounds, workspace));
                    Window::Current->Activate();
                }
            } else {
                workspace = ref new WorkSpace();
                this->Win2DAppMain(self, workspace);
            
                if (e->PreviousExecutionState == ApplicationExecutionState::Terminated) {
                    // TODO: Restore the saved session state only when appropriate, scheduling the
                    // final launch steps after the restore is complete
                }

                if (e->PrelaunchActivated == false) {
                    workspace->initialize_component(adjusted_workspace_size(self->VisibleBounds, workspace));
                    Window::Current->Content = workspace;
                    Window::Current->Activate();
                }
            }
        }

    private:
        void OnSuspending(Object^ sender, SuspendingEventArgs^ e) {
            // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
            auto workspace = dynamic_cast<WorkSpace^>(Window::Current->Content);
            if (workspace != nullptr) workspace->suspend(e->SuspendingOperation);
        }

        void DoResize(ApplicationView^ view, Object^ obj) {
            auto workspace = dynamic_cast<WorkSpace^>(Window::Current->Content);
            if (workspace != nullptr) {
                auto region = adjusted_workspace_size(view->VisibleBounds, workspace);
                workspace->reflow(region.Width, region.Height);
            }
        }
    };
}

int main(Platform::Array<Platform::String^>^ args) {
    auto lazy_main = [](ApplicationInitializationCallbackParams^ p) { ref new WarGrey::Win2DDemo::Win2DApp(); };
    Application::Start(ref new ApplicationInitializationCallback(lazy_main));
}
