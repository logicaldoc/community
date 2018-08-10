package com.logicaldoc.core.rss;

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.communication.Message;
import com.logicaldoc.core.communication.Recipient;
import com.logicaldoc.core.communication.SystemMessage;
import com.logicaldoc.core.communication.SystemMessageDAO;
import com.logicaldoc.core.rss.dao.FeedMessageDAO;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.task.Task;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * This task retrieve and store all feeds from an Url.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class ProductNews extends Task {
	public static final String NAME = "ProductNews";

	private FeedMessageDAO feedMessageDao;

	private ContextProperties config;

	private long saved = 0;

	private long errors = 0;

	private long size = 0;

	public ProductNews() {
		super(NAME);
		log = LoggerFactory.getLogger(ProductNews.class);
	}

	@Override
	public boolean isIndeterminate() {
		return false;
	}

	@Override
	public boolean isConcurrent() {
		return true;
	}

	@Override
	protected void runTask() throws Exception {
		log.info("Start retrieving news");

		try {

			// Clean the DB from feed messages older that 1 year.
			feedMessageDao.deleteOld();

			ContextProperties config = Context.get().getProperties();
			String url = config.getProperty("news.url");
			if (StringUtils.isEmpty(url))
				url = "https://www.logicaldoc.com/news/rss";
			if (!"-".equals(url))
				parseFeed(url);
		} finally {
			log.info("Retrieving news finished");
			log.info("Retrieved news: " + saved);
			log.info("Errors: " + errors);
		}
	}

	private void parseFeed(String url) {
		FeedParser parser = new FeedParser(url);
		Feed feed = parser.readFeed();
		// First of all feed messages to be saved
		size = feed.getMessages().size();

		log.info("Found a total of " + size + " feed messages to be processed");

		Calendar cal = Calendar.getInstance();
		cal.add(Calendar.YEAR, -1);
		Date oldestDate = cal.getTime();

		for (FeedMessage message : feed.getMessages()) {
			try {
				log.debug("Parsing message " + message.getTitle());
				if(message.getPubDate()==null || message.getPubDate().before(oldestDate)){
					log.debug("Skipping old message " + message.getTitle());
					continue;
				}
				
				if (feedMessageDao.findByGuid(message.getGuid()) == null) {
					// The parsing message is not already saved into the
					// database, so we save it.
					feedMessageDao.store(message);
					saved++;
				}
				log.debug("Parsed message " + message.getTitle());
			} catch (Throwable e) {
				log.error(e.getMessage(), e);
				errors++;
			} finally {
				next();
			}
			if (interruptRequested)
				return;
		}
		if (saved > 0)
			createAlerts();
	}

	@Override
	protected String prepareReport(Locale locale) {
		StringBuffer sb = new StringBuffer();
		if (saved > 0)
			sb.append(I18N.message("feednewsfound", locale, new Object[] { saved }));
		else
			sb.append(I18N.message("feednewsnotfound", locale));
		sb.append("\n");
		sb.append(I18N.message("errors", locale) + ": ");
		sb.append(errors);
		return sb.toString();
	}

	/**
	 * Creates the system message for the administrator user.
	 */
	private void createAlerts() {
		SystemMessageDAO systemMessageDao = (SystemMessageDAO) Context.get().getBean(SystemMessageDAO.class);
		Map<Locale, Set<Recipient>> recipientsLocalesMap = new HashMap<Locale, Set<Recipient>>();
		try {
			for (Long userId : userDao.findAllIds()) {
				User user = userDao.findById(userId);
				if (user.isMemberOf("admin")) {
					Recipient recipient = new Recipient();
					recipient.setName(user.getUsername());
					recipient.setAddress(user.getUsername());
					recipient.setType(Recipient.TYPE_SYSTEM);
					recipient.setMode(Recipient.MODE_EMAIL_TO);
					recipient.setRead(1);

					// Add the recipient to the recipients list according to the
					// user Locale
					if (recipientsLocalesMap.containsKey(user.getLocale())) {
						recipientsLocalesMap.get(user.getLocale()).add(recipient);
					} else {
						Set<Recipient> recipients = new HashSet<Recipient>();
						recipients.add(recipient);
						recipientsLocalesMap.put(user.getLocale(), recipients);
					}
				}
			}

			for (Locale locale : recipientsLocalesMap.keySet()) {
				SystemMessage message = new SystemMessage();
				message.setType(Message.TYPE_NOTIFICATION);
				message.setAuthor("SYSTEM");
				message.setSentDate(new Date());
				message.setRecipients(recipientsLocalesMap.get(locale));
				message.setSubject(I18N.message("feednewsfound", locale, new Object[] { saved }));
				message.setMessageText(I18N.message("productnewsmessage", locale));

				systemMessageDao.store(message);
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
	}

	public FeedMessageDAO getFeedMessageDao() {
		return feedMessageDao;
	}

	public void setFeedMessageDao(FeedMessageDAO feedMessageDao) {
		this.feedMessageDao = feedMessageDao;
	}

	public ContextProperties getConfig() {
		return config;
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
	}
}
