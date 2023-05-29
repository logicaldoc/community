package com.logicaldoc.webservice;

import java.io.InputStream;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.ResourceBundle;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.binding.soap.SoapMessage;
import org.apache.cxf.common.i18n.BundleUtils;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.interceptor.Interceptor;
import org.apache.cxf.io.CachedOutputStream;
import org.apache.cxf.message.Message;
import org.apache.cxf.phase.AbstractPhaseInterceptor;
import org.apache.cxf.phase.Phase;
import org.apache.cxf.transport.http.AbstractHTTPDestination;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.RunLevel;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.sequence.SequenceDAO;
import com.logicaldoc.core.threading.ThreadPools;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.Pair;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.time.TimeDiff;
import com.logicaldoc.util.time.TimeDiff.TimeField;

/**
 * An {@link Interceptor} invoked when a webservice message has been received
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class WebserviceInterceptor extends AbstractPhaseInterceptor<Message> {

	public static final String THREADPOOL_CALL_COUNTER = "WebserviceCallCounter";

	public static final String THREADPOOL_CALL_STORE = "WebserviceCallStore";

	private static final String WSCALL = "wscall";

	public static final String WSCALL_HYPHEN = WSCALL + "-";

	protected static Logger log = LoggerFactory.getLogger(WebserviceInterceptor.class);

	private SequenceDAO sequenceDAO;

	/**
	 * A cache of counters: key=countername-tenantId name value=actual total
	 * calls
	 */
	private static ConcurrentHashMap<Pair<String, Long>, AtomicLong> counters = new ConcurrentHashMap<>();

	/**
	 * Last time a database synchronization of the coutners has done
	 */
	private Date lastSync;

	/**
	 * Last time the oldest calls were cleaned
	 */
	private Date lastClean;

	private ContextProperties settings;

	public WebserviceInterceptor() {
		super(Phase.RECEIVE);
	}

	@Override
	public void handleMessage(Message message) throws Fault {
		if (!settings.getBoolean("webservice.enabled", false)) {
			final ResourceBundle BUNDLE = BundleUtils.getBundle(WebserviceInterceptor.class);
			throw new Fault(new org.apache.cxf.common.i18n.Message("Webservices not enabled", BUNDLE));
		}

		String payload = getPayload(message, 2048L);

		Session session = null;
		try {
			session = getSession(message, payload);
		} catch (Exception t) {
			log.warn(t.getMessage(), t);
		}

		/**
		 * Increase the counters
		 */
		try {
			increaseCounters(session);
		} catch (Exception t) {
			log.warn(t.getMessage(), t);
		}

		try {
			if (RunLevel.current().aspectEnabled(WebserviceCall.ASPECT)
					&& settings.getBoolean("webservice.call.gridRecord", false)) {
				WebserviceCall call = new WebserviceCall();
				call.setTenantId(Tenant.SYSTEM_ID);

				if (Context.get().getProperties().getBoolean("webservice.call.gridRecord.payload", false)) {
					/*
					 * Retrieve the full payload
					 */
					payload = getPayload(message, null);
					call.setPayload(payload);
				} else
					call.setPayload(null);

				HttpServletRequest req = (HttpServletRequest) message.get(AbstractHTTPDestination.HTTP_REQUEST);
				if (req != null)
					call.setIp(req.getRemoteAddr());

				if (session != null)
					call.setSession(session);

				saveCall(call, message);
			}
		} catch (Exception t) {
			log.warn(t.getMessage(), t);
		}
	}

	public void saveCall(WebserviceCall call, Message message) {
		call.setProtocol(message instanceof SoapMessage ? WebserviceCall.SOAP : WebserviceCall.REST);
		String uri = (String) message.get(Message.REQUEST_URL);
		if (uri != null) {
			String query = (String) message.get(Message.QUERY_STRING);
			if (query != null)
				uri += "?" + query;
		}

		call.setUri(uri);

		// Strip the <content> tags in the payload and mask the credentials
		if (call.getPayload() != null) {
			call.setPayload(stripFileContent(call.getPayload()));
			call.setPayload(maskCredentials(call.getPayload()));
		}

		if (call.getPayload() != null && call.getPayload().length() > 4000)
			call.setPayload(StringUtils.abbreviate(call.getPayload(), 4000));

		if (call.getSessionId() != null) {
			String sid = call.getSessionId();
			String maskedSid = StringUtils.overlay(sid, StringUtils.repeat("X", sid.length() - 6), 0, sid.length() - 6);
			if (call.getUri() != null)
				call.setUri(call.getUri().replace(sid, maskedSid));
			if (call.getPayload() != null)
				call.setPayload(call.getPayload().replace(sid, maskedSid));
		}

		if (call.getUri() != null)
			call.setUri(maskCredentials(call.getUri()));

		ThreadPools pools = (ThreadPools) Context.get().getBean(ThreadPools.class);
		pools.schedule(new WebserviceCallStore(call), THREADPOOL_CALL_STORE, 5000);
	}

	static String maskCredentials(String originalString) {
		String maskedString = originalString;

		String pattern = "<password(.*)>([\\w\\-\\d]*)</password>";
		Pattern regPat = Pattern.compile(pattern);
		Matcher matcher = regPat.matcher(maskedString);
		while (matcher.find()) {
			String matchedText = matcher.group();
			if (!matchedText.equals(originalString))
				maskedString = maskedString.replace(matchedText, "<password>XXXX</password>");
		}

		pattern = "password=[^&.]*";
		regPat = Pattern.compile(pattern);
		matcher = regPat.matcher(maskedString);
		while (matcher.find()) {
			String matchedText = matcher.group();
			if (!matchedText.equals(maskedString))
				maskedString = maskedString.replace(matchedText, "password=XXXX");
		}

		return maskedString;
	}

	/**
	 * Strips the file contents, generally encapsulated in an element called
	 * <content>
	 * 
	 * @param originalString the original XML content
	 * 
	 * @return the stripped XML
	 */
	static String stripFileContent(String originalString) {
		return originalString.replaceAll("<content[^>]*>.*</content>", "<content>...</content>");
	}

	/**
	 * Increases the counters
	 * 
	 * @param session the current session if any
	 */
	protected void increaseCounters(Session session) {
		String currentMonth = getCurrentMonth();
		increaseCounter(WSCALL, Tenant.SYSTEM_ID);
		increaseCounter(WSCALL_HYPHEN + currentMonth, Tenant.SYSTEM_ID);
		if (session != null) {
			increaseCounter(WSCALL, session.getTenantId());
			increaseCounter(WSCALL_HYPHEN + currentMonth, session.getTenantId());
		}

		Date now = new Date();
		if (lastSync == null)
			lastSync = new Date(1);

		long timeSinceLastSync = ChronoUnit.MINUTES.between(lastSync.toInstant(), now.toInstant());
		if (timeSinceLastSync >= 10) {
			ThreadPools pools = (ThreadPools) Context.get().getBean(ThreadPools.class);
			pools.schedule(new WebserviceCallCounterSync(), THREADPOOL_CALL_COUNTER, 5000);
		}
	}

	protected void syncCounters() {
		for (Pair<String, Long> counterPair : counters.keySet()) {
			AtomicLong counter = counters.get(counterPair);
			sequenceDAO.next(counterPair.getKey(), 0L, counterPair.getValue(), counter.get());
			counter.set(0L);
		}
		lastSync = new Date();
	}

	public void shutdown() {
		try {
			syncCounters();
		} catch (Exception t) {
			log.warn(t.getMessage(), t);
		}
	}

	protected void increaseCounter(String counterName, long tenantId) {
		Pair<String, Long> counterPair = new Pair<>(counterName, tenantId);
		AtomicLong counter = counters.get(counterPair);
		if (counter == null) {
			counter = new AtomicLong(0L);
			counters.put(counterPair, counter);
		}
		counter.incrementAndGet();
	}

	static Session getSession(Message message, String payload) {
		HttpServletRequest request = (HttpServletRequest) message.get(AbstractHTTPDestination.HTTP_REQUEST);
		Session session = SessionManager.get().getSession(request);
		if (session != null)
			return session;

		if (StringUtils.isEmpty(payload))
			payload = getPayload(message, 2048L);
		if (StringUtils.isNotEmpty(payload)) {
			String pattern = "<sid(.*)>([\\w\\-\\d]*)</sid>";
			Pattern regPat = Pattern.compile(pattern);
			Matcher matcher = regPat.matcher(payload);
			if (matcher.find()) {
				String matchedText = matcher.group();
				String sid = matchedText.substring(matchedText.indexOf('>') + 1, matchedText.lastIndexOf('<'));
				session = SessionManager.get().get(sid);
			}

		}
		return session;
	}

	static String getPayload(Message message, Long maxlength) throws Fault {
		Object contentType = message.getContextualProperty(Message.CONTENT_TYPE);
		if (contentType == null || !contentType.toString().toLowerCase().startsWith("text/"))
			return null;

		try (InputStream is = message.getContent(InputStream.class);
				CachedOutputStream bos = new CachedOutputStream()) {
			if (is != null) {
				IOUtils.copy(is, bos);

				bos.flush();
				
				StringBuilder sb = new StringBuilder();
				message.setContent(InputStream.class, bos.getInputStream());
				if(maxlength!=null)
					bos.writeCacheTo(sb, maxlength);
				else
					bos.writeCacheTo(sb);

				return sb.toString();
			}
		} catch (Exception e) {
			throw new Fault(e);
		}
		return null;
	}

	private static String getCurrentMonth() {
		LocalDate date = LocalDate.now();
		StringBuilder sb = new StringBuilder(Integer.toString(date.getYear()));
		int month = date.getMonthValue();
		if (month < 10)
			sb.append("0");
		sb.append(Integer.toString(month));
		return sb.toString();
	}

	/**
	 * This runnable takes care of synchronizing the counters into the database
	 */
	class WebserviceCallCounterSync implements Runnable {

		@Override
		public void run() {
			try {
				syncCounters();
				lastSync = new Date();
			} catch (Exception t) {
				log.warn(t.getMessage(), t);
			}
		}
	}

	/**
	 * This runnable takes care of writing a call into the database
	 */
	class WebserviceCallStore implements Runnable {

		private WebserviceCall call;

		public WebserviceCallStore(WebserviceCall call) {
			super();
			this.call = call;
		}

		@Override
		public void run() {
			try {
				WebserviceCallDAO dao = (WebserviceCallDAO) Context.get().getBean(WebserviceCallDAO.class);
				Date now = new Date();

				if (lastClean == null)
					lastClean = now;

				long timeSinceLastClean = TimeDiff.getTimeDifference(lastClean, now, TimeField.HOUR);
				if (timeSinceLastClean >= 24)
					dao.cleanOldCalls(settings.getInt("webservice.call.ttl", 90));

				dao.store(call);
			} catch (Exception t) {
				log.warn(t.getMessage(), t);
			}
		}
	}

	public void setSequenceDAO(SequenceDAO sequenceDAO) {
		this.sequenceDAO = sequenceDAO;
	}

	public void setSettings(ContextProperties settings) {
		this.settings = settings;
	}
}