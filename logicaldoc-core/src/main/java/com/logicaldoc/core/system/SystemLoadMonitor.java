package com.logicaldoc.core.system;

import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.lang.management.OperatingSystemMXBean;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import org.apache.commons.collections4.queue.CircularFifoQueue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.SystemUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.exec.Exec;

/**
 * This class monitors the system load and notifies the listeners accordingly
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7.1
 */
public class SystemLoadMonitor {

	protected static Logger log = LoggerFactory.getLogger(SystemLoadMonitor.class);

	private ContextProperties config;

	private CircularFifoQueue<Integer> samples = null;

	private int averageCpuLoad = 0;

	private LoadTracker tracker = new LoadTracker();

	private List<SystemLoadListener> listeners = new ArrayList<>();

	private boolean lastCheckOverloaded = false;

	public void addListener(SystemLoadListener listener) {
		if (!listeners.contains(listener))
			listeners.add(listener);
	}

	public void removeListener(SystemLoadListener listener) {
		listeners.remove(listener);
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
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
		// try to get the CPU usage with JMX
		OperatingSystemMXBean osMXBean = ManagementFactory.getOperatingSystemMXBean();
		int load = (int) Math.round((osMXBean.getSystemLoadAverage() / osMXBean.getAvailableProcessors()) * 100d);
		if (load < 0) {
			// On some systems Java is not able to get the CPU usage so try to
			// extract the information from the shell
			load = SystemUtil.isWindows() ? getCpuLoadOnWindows() : getCpuLoadOnLinux();
		}
		if (load < 0)
			load = 0;

		if (log.isTraceEnabled())
			log.trace("Got CPU load: {}", load);

		return load;
	}

	private int getCpuLoadOnWindows() {
		StringBuilder sb = new StringBuilder();
		try {
			Exec exec = new Exec();
			exec.setOutPrefix(null);
			exec.exec("wmic cpu get loadpercentage", null, null, sb, 5);
		} catch (IOException e1) {
			log.warn(e1.getMessage(), e1);
			return 0;
		}

		try {
			Thread.sleep(500);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}

		/*
		 * Typical output is: LoadPercentage 6
		 */
		String output = sb.toString();
		int load = 0;

		try (Scanner scanner = new Scanner(output)) {
			while (scanner.hasNextLine()) {
				// trim and remove all non printable chars
				String line = scanner.nextLine().trim().replaceAll("\\p{Zs}+", "");
				if (line.matches("^\\d+$")) {
					load = Integer.parseInt(line);
					break;
				}
			}
		}

		return load;
	}

	private int getCpuLoadOnLinux() {
		StringBuilder sb = new StringBuilder();
		try {
			Exec exec = new Exec();
			exec.setOutPrefix(null);
			exec.exec("top -bn1", null, null, sb, 5);
		} catch (IOException e1) {
			log.warn(e1.getMessage(), e1);
			return 0;
		}

		try {
			Thread.sleep(500);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}

		/*
		 * Typical output is: %Cpu(s): 98.5 us, 1.5 sy, 0.0 ni, 0.0 id, 0.0 wa,
		 * 0.0 hi, 0.0 si, 0.0 ste
		 */
		String output = sb.toString();
		int load = 0;

		try (Scanner scanner = new Scanner(output)) {
			while (scanner.hasNextLine()) {
				// trim and remove all non printable chars
				String line = scanner.nextLine().trim().toLowerCase().replaceAll("\\p{Zs}+", "");
				if (line.startsWith("%cpu")) {
					line = line.substring(line.indexOf(':') + 1).trim().toLowerCase();
					line = line.substring(0, line.indexOf('u')).trim();
					load = Math.round(Float.parseFloat(line));
					break;
				}
			}
		}

		return load;
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

	public void stop() {
		if (tracker != null)
			try {
				tracker.end();
			} catch (Exception e) {
				// Nothing to do
			}
	}

	public void start() {
		initSamples();
		tracker.setPriority(Thread.MIN_PRIORITY);
		tracker.start();
		log.info("System load monitor started");
	}

	/**
	 * Get a sample and stores it in the FIFO of samples, each time also
	 * calculates the average CPU load
	 */
	private void pickCpuLoad() {
		// Get an actual sample and store it
		int sample = getCpuLoad();
		samples.add(sample);

		// Calculate and save the average load
		averageCpuLoad = (int) Math.round(samples.stream().mapToDouble(s -> Double.valueOf(s)).average().getAsDouble());

		if (log.isTraceEnabled())
			log.trace("Average CPU load: {}", averageCpuLoad);
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
	}
}