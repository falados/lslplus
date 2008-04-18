package lslplus;

import lslplus.sim.SimStatuses;

public interface SimListener {

    public void newLogMessages(SimStatuses.Message[] messages);
    
    public void newSimState(SimStatuses.SimState state);
    
    public void simLaunched();
    
    public void simEnded();
}
