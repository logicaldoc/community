package com.logicaldoc.core.system;

/**
 * Concrete implementations have to react to changes in the system load.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7.1
 */
public interface SystemLoadListener {
	/**
	 * Invoked when the average whole system's CPU usage is higher than the threshold specified in the global parameter'system.cpuload.max'
	 *  
	 * @param avgSysCpuLoad The average system CPU usage
	 * @param avgJvmCpuLoad The average JVM CPU usage
	 */
	public void onOverload(int avgSysCpuLoad, int avgJvmCpuLoad); 

	/**
	 * Invoked when the average whole system's CPU usage is higher than the threshold specified in the global parameter'system.cpuload.max'
	 *  
	 * @param avgSysCpuLoad The average system CPU usage
	 * @param avgJvmCpuLoad The average JVM CPU usage
	 */
	public void onUnderload(int avgSysCpuLoad, int avgJvmCpuLoad);
}
