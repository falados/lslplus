package lslplus.preferences;

import java.io.IOException;

import lslplus.LslPlusPlugin;
import lslplus.LslProjectNature;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public class LslPlusPreferencePage extends FieldEditorPreferencePage implements
        IWorkbenchPreferencePage {

    private static final String LSL_PLUS_PREFERENCE_PAGE_ENABLE_OPTIMIZATIONS = "LslPlusPreferencePage.ENABLE_OPTIMIZATIONS"; //$NON-NLS-1$
    private static final String LSLPLUS_EXECUTABLE_PATH = "LslPlusPreferencePage.LSLPlusExecutablePath"; //$NON-NLS-1$

    public LslPlusPreferencePage() throws IOException {
        setPreferenceStore(LslPlusPlugin.getDefault().getPreferenceStore());
    }

    public void init(IWorkbench workbench) {
    }

    protected IPreferenceStore doGetPreferenceStore() {
        return LslPlusPlugin.getDefault().getPreferenceStore();
    }

    protected void createFieldEditors() {
        addField(new FileFieldEditor(LslPlusPlugin.LSLPLUS_NATIVE_PATH,
                Messages.getString(LSLPLUS_EXECUTABLE_PATH), getFieldEditorParent())); 
        addField(new BooleanFieldEditor(LslProjectNature.OPTIMIZE,
                Messages.getString(LSL_PLUS_PREFERENCE_PAGE_ENABLE_OPTIMIZATIONS), getFieldEditorParent()));
    }

}
