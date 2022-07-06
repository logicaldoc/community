package com.logicaldoc.util.event;

public abstract class SystemEventListener {
	
	private SystemEventStatus systemStatus;
	
	public SystemEventListener(SystemEventStatus status) {
		if(status == null)
			throw new RuntimeException("A valid SystemEventStatus must be registered");
		
		this.systemStatus = status;
	}
	
	public SystemEventStatus getSystemStatus() {
		return systemStatus;
	}
	
	public abstract void processEvent();
	
}
