package lslplus.launching;

import lslplus.LslPlusPlugin;
import lslplus.LslProjectNature;
import lslplus.debug.LslDebugTarget;
import lslplus.debug.LslSimProcess;
import lslplus.debug.LslSourceLocator;
import lslplus.util.Util;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;
import org.eclipse.jface.dialogs.MessageDialog;

public class SimLaunchDelegate implements ILaunchConfigurationDelegate {
    private static final String BLANK = ""; //$NON-NLS-1$
    public void launch(ILaunchConfiguration configuration, String mode, ILaunch launch,
            IProgressMonitor monitor) throws CoreException {
        Util.log("launch!!!"); //$NON-NLS-1$

        String fullPath = configuration.getAttribute(LaunchLslTestShortcut.LC_RESOURCE_NAME, BLANK);
        Path path = new Path(fullPath);
        IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember(path);
        if (resource == null) {
            MessageDialog.openError(null, "Resource no longer exists",
                    "Referenced resource no longer exists!");
            return;
        }

        String name = LslProjectNature.resourceToLslPlusName(resource);
        
        LslProjectNature nature = (LslProjectNature) resource.getProject().getNature(LslProjectNature.ID);
        String sourceDescriptor = nature.projectSourceList();
        String testDescriptor = "<sim-descriptor>" + sourceDescriptor + //$NON-NLS-1$
                                "<worldDef><script>" + name + "</script></worldDef>" + //$NON-NLS-1$ //$NON-NLS-2$
                                "</sim-descriptor>";  //$NON-NLS-1$
        if (LslPlusPlugin.DEBUG) Util.log(testDescriptor);
        LslSimProcess p = new LslSimProcess(testDescriptor, launch);
        LslDebugTarget target = new LslDebugTarget("lslplus-test", launch, p); //$NON-NLS-1$
        p.go();
        LslPlusPlugin.getDefault().getSimManager().simLaunched(p);
        launch.addDebugTarget(target);
        launch.addProcess(p);
        launch.setSourceLocator(new LslSourceLocator());
    }

}
