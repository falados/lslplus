package lslplus.wizards;

import org.eclipse.ui.INewWizard;

public class NewAvEventHandler extends NewFileSampleWizard implements
        INewWizard {

    public NewAvEventHandler() {
        super("New Avatar Event Handler", "samples/AvEventHandler.lslm");
    }

}
