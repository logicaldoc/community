package com.logicaldoc.core.system;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.config.ContextProperties;

/**
 * This class monitors the system load and notifies the listeners accordingly
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7.1
 */
public class SystemLoadMonitor {
	protected static Logger log = LoggerFactory.getLogger(SystemLoadMonitor.class);

	private ContextProperties config;

	private int[][] samples = new int[60][60];

	private int[] averageCpuLoad = new int[2];

	private LoadTracker tracker = new LoadTracker();

	private List<SystemLoadListener> listeners = new ArrayList<>();

	public void setConfig(ContextProperties config) {
		this.config = config;
		setSamplesTotal(config.getInt("load.cpusamples"));
	}

	public void setSamplesTotal(int samplesTotal) {
		samples = new int[samplesTotal][samplesTotal];
		averageCpuLoad = new int[2];
	}

	public void addListener(SystemLoadListener listener) {
		if (!listeners.contains(listener))
			listeners.add(listener);
	}

	public void removeListener(SystemLoadListener listener) {
		listeners.remove(listener);
	}

	/**
	 * Gets the average CPU load computed in the last two minutes:<br>
	 * <ol>
	 * <li>first value: the 'average cpu usage' for the whole system</li>
	 * <li>second value: the 'average cpu usage' for the JVM process</li>
	 * </ol>
	 * 
	 * @return average CPU load infos
	 */
	public int[] getAverageCpuLoad() {
		return averageCpuLoad;
	}

	/**
	 * Check if the 'recent' CPU load is over the limit defined in 'load.cpumax'
	 * config parameter.
	 * 
	 * @return if the CPU is overloaded
	 */
	public boolean isCpuOverLoaded() {
		int cpumax = config.getInt("load.cpumax", 50);
		if (cpumax < 1)
			return false;
		else
			return getCpuLoad()[0] > cpumax;
	}

	/**
	 * Retrieve the CPU load:<br>
	 * <ol>
	 * <li>first value: the 'recent cpu usage' for the whole system</li>
	 * <li>second value: the 'recent cpu usage' for the JVM process</li>
	 * </ol>
	 * 
	 * @return current CPU load infos
	 */
	public int[] getCpuLoad() {
		return new int[] { 0, 0 };
	}

	/**
	 * Check if the 'average' CPU load is over the limit defined in
	 * 'system.cpuload.max' config parameter.
	 * 
	 * @return if the CPU is overloaded
	 */
	public boolean isAverageCpuOverLoaded() {
		int cpumax = config.getInt("load.cpumax", 50);
		if (cpumax < 1)
			return false;
		else
			return averageCpuLoad[0] > cpumax;
	}

	public void stop() {
		if (tracker != null)
			try {
				tracker.end();
			} catch (Exception e) {
				// Nothing to do
			}
	}

	public void start() {
		stop();
		tracker.setPriority(Thread.MIN_PRIORITY);
		tracker.start();
	}

	/*
	 * This thread collects statistics about the system load in the last two
	 * minutes
	 */
	class LoadTracker extends Thread {

		private boolean running = true;

		public LoadTracker() {
		}

		public void end() {
			running = false;
		}

		@Override
		public void run() {
			while (running) {
				synchronized (SystemLoadMonitor.this) {
					int i = 0;
					while (i < samples.length && running) {
						try {
							SystemLoadMonitor.this.wait(2000);
						} catch (InterruptedException e) {
							Thread.currentThread().interrupt();
						}

						// Get an actual sample and store it
						int[] sample = getCpuLoad();
						samples[i] = sample;

						long totals[] = new long[2];

						// Compute the average
						for (int j = 0; j < samples.length; j++) {
							totals[0] = totals[0] + samples[j][0];
							totals[1] = totals[1] + samples[j][1];
						}

						int[] averageOld = new int[] { averageCpuLoad[0], averageCpuLoad[1] };
						averageCpuLoad[0] = (int) Math.round(totals[0] / (float) samples.length);
						averageCpuLoad[1] = (int) Math.round(totals[1] / (float) samples.length);

						int cpuLoadMax = config.getInt("system.cpuload.max", 50);
						if (cpuLoadMax > 0)
							if (averageOld[0] <= cpuLoadMax && averageCpuLoad[0] > cpuLoadMax) {
								log.warn("The system is overloaded (" + averageCpuLoad[0] + "%)");
								for (SystemLoadListener listener : listeners) {
									listener.onOverload(averageCpuLoad[0], averageCpuLoad[1]);
								}
							} else if (averageOld[0] > cpuLoadMax && averageCpuLoad[0] <= cpuLoadMax) {
								log.warn("The system is underloaded (" + averageCpuLoad[0] + "%)");
								for (SystemLoadListener listener : listeners) {
									listener.onUnderload(averageCpuLoad[0], averageCpuLoad[1]);
								}
							}
						i++;
					}
				}
			}
		}
	}
}