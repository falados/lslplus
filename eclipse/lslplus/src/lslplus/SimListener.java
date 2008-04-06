package lslplus;

import lslplus.sim.SimStatuses;

public interface SimListener {

    public void newLogMessages(SimStatuses.Message[] messages);
    
    public void simLaunched();
    
    public void simEnded();
}
