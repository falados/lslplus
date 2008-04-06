package lslplus.debug;

import lslplus.LslPlusPlugin;
import lslplus.launching.Messages;

import org.eclipse.debug.core.ILaunch;

public class LslTestProcess extends LslProcess {
    private static final String UNIT_TESTER_EXE = "UnitTester2.exe"; //$NON-NLS-1$
    private String descriptor;
    public LslTestProcess(String descriptor, ILaunch launch) {
        super(launch);
        this.descriptor = descriptor;
    }
    
    protected Interactor createInteractor(Process p) {
        return new LslTestInteractor(launch.getLaunchMode(),LslPlusPlugin.getDefault().getTestManager(),
                descriptor, p.getInputStream(), p.getOutputStream());
    }
    
    protected Process launchExecutable() {
        return LslPlusPlugin.launchExecutable(UNIT_TESTER_EXE, false);
    }
    
    public String getLabel() {
        return Messages.getString("TestLaunchDelegate.TEST"); //$NON-NLS-1$
    }

}
