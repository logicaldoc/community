package com.logicaldoc.core.rss;

public class ReadTest {
	public static void main(String[] args) {
//		FeedParser parser = new FeedParser("http://www.logicaldoc.com/news/rss.html");
		FeedParser parser = new FeedParser("http://www.logicaldoc.com/version/rss.html");
			//new FeedParser("http://www.vogella.de/article.rss");
		Feed feed = parser.readFeed();
		System.out.println(feed);
		for (FeedMessage message : feed.getMessages()) {
			System.out.println("--------------------------------------------");
			System.out.println(message.getTitle());
			System.out.println(message.getLink());
			System.out.println(message.getDescription());		
		}
	}
}
