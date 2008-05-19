package lslplus.wizards;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

import lslplus.LslPlusPlugin;
import lslplus.util.Util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

public class NewSimEventHandler extends Wizard implements INewWizard {
	private LslModuleWizardPage mainPage;
	private IStructuredSelection selection;
	private class LslModuleWizardPage extends LslFileCreationWizardPage {
		public LslModuleWizardPage(IStructuredSelection selection) {
			super("createModule", selection); //$NON-NLS-1$
			setTitle("New Sim Event Handler");
			setPageComplete(false);
			setFileExtension("lslm"); //$NON-NLS-1$
			setDefaultPageImageDescriptor(image());
			
		}

		protected InputStream getInitialContents() {
		    try {
                return FileLocator.openStream(LslPlusPlugin.getDefault().getBundle(),
                        new Path("samples/DefaultEventHandler.lslm"), false);//$NON-NLS-1$
            } catch (IOException e) {
                Util.log(e, e.getLocalizedMessage());
                return new ByteArrayInputStream("$module ()\n// can't find template!".getBytes());
            } 
		}

		protected IStatus validateFileName(String fileName) {
			return new Status(IStatus.OK,  "lslplus",""); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	public NewSimEventHandler() {
		this.setDefaultPageImageDescriptor(image());
	}

	private static ImageDescriptor image() {
		return Util.findDescriptor("$nl$/icons/new_test.png"); //$NON-NLS-1$
	}

	public boolean performFinish() {
        IFile f = mainPage.createNewFile();
        LslPlusPlugin.openResource(getShell(), f);
		return true;
	}

	public void addPages() {
		super.addPages();
		mainPage = new LslModuleWizardPage(selection);
		addPage(mainPage);
	}

	public void init(IWorkbench workbench, IStructuredSelection selection) {
		this.selection = selection;
	}
}