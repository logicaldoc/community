package com.logicaldoc.core.system;

import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.util.ArrayList;
import java.util.List;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.management.MBeanServerConnection;

import org.apache.commons.collections4.queue.CircularFifoQueue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.logicaldoc.util.config.ContextProperties;
import com.sun.management.OperatingSystemMXBean;

/**
 * This class monitors the system load and notifies the listeners accordingly
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7.1
 */
@Component("systemLoadMonitor")
public class SystemLoadMonitor {

	protected static Logger log = LoggerFactory.getLogger(SystemLoadMonitor.class);

	private ContextProperties config;

	private CircularFifoQueue<Integer> samples = null;

	private int averageCpuLoad = 0;

	private LoadTracker tracker = new LoadTracker();

	private List<SystemLoadListener> listeners = new ArrayList<>();

	private boolean lastCheckOverloaded = false;

	@Autowired
	public SystemLoadMonitor(ContextProperties config) {
		super();
		this.config = config;
	}

	public void addListener(SystemLoadListener listener) {
		if (!listeners.contains(listener))
			listeners.add(listener);
	}

	public void removeListener(SystemLoadListener listener) {
		listeners.remove(listener);
	}

	private void initSamples() {
		int wantedSamples = config.getInt("load.cpusamples", 60);

		if (samples == null || samples.maxSize() != wantedSamples) {
			CircularFifoQueue<Integer> newSamples = new CircularFifoQueue<>(wantedSamples);

			/// Init the samples with all zeros
			for (int i = 0; i < wantedSamples; i++)
				newSamples.add(0);

			// Copy the current samples
			if (samples != null)
				newSamples.addAll(samples);
			samples = newSamples;
		}
	}

	/**
	 * Retrieve the CPU load.
	 * 
	 * @return current CPU load
	 */
	public int getCpuLoad() {
		try {
			MBeanServerConnection mbsc = ManagementFactory.getPlatformMBeanServer();

			OperatingSystemMXBean osMBean = ManagementFactory.newPlatformMXBeanProxy(mbsc,
					ManagementFactory.OPERATING_SYSTEM_MXBEAN_NAME, OperatingSystemMXBean.class);

			int loadPErcentage = (int) Math.ceil(osMBean.getCpuLoad() * 100D);
			if (log.isTraceEnabled())
				log.trace("Got CPU load {}%", loadPErcentage);

			return loadPErcentage;
		} catch (IOException e) {
			return 0;
		}
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
			return averageCpuLoad > cpumax;
	}

	@PostConstruct
	public void start() {
		initSamples();
		tracker.setPriority(Thread.MIN_PRIORITY);
		tracker.start();
		log.info("System load monitor started");
	}

	@PreDestroy
	public void stop() {
		if (tracker != null)
			try {
				tracker.end();
			} catch (Exception e) {
				// Nothing to do
			}
	}

	/*
	 * This thread collects statistics about the system load in the last time
	 * (samples number * 1sec)
	 */
	class LoadTracker extends Thread {

		private boolean running = true;

		public boolean isRunning() {
			return running;
		}

		public void end() {
			running = false;
		}

		@Override
		public void run() {
			while (running) {
				initSamples();
				int i = 0;
				while (i < samples.maxSize() && running) {
					waitMilliseconds(1000);

					pickCpuLoad();

					checkOverloadOrUnderload();

					i++;
				}
			}
		}

		private void checkOverloadOrUnderload() {
			if (isAverageCpuOverLoaded()) {
				if (!lastCheckOverloaded)
					log.warn("The system is overloaded ({}%)", averageCpuLoad);
				lastCheckOverloaded = true;

				for (SystemLoadListener listener : listeners) {
					listener.onOverload(averageCpuLoad, averageCpuLoad);
				}
			} else {
				if (lastCheckOverloaded)
					log.warn("The system is underloaded ({}%)", averageCpuLoad);
				lastCheckOverloaded = false;

				for (SystemLoadListener listener : listeners) {
					listener.onUnderload(averageCpuLoad, averageCpuLoad);
				}
			}
		}

		private void waitMilliseconds(long ms) {
			try {
				Thread.sleep(ms);
			} catch (InterruptedException e) {
				end();
				Thread.currentThread().interrupt();
			}
		}

		/**
		 * Gets a sample and stores it in the FIFO of samples, each time also
		 * calculates the average CPU load
		 */
		private void pickCpuLoad() {
			// Get an actual sample and store it
			int sample = getCpuLoad();
			samples.add(sample);

			// Calculate and save the average load
			averageCpuLoad = (int) Math.round(samples.stream().mapToDouble(Double::valueOf).average().getAsDouble());

			if (log.isTraceEnabled())
				log.trace("Average CPU load: {}", averageCpuLoad);
		}
	}
}