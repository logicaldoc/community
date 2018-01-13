package com.logicaldoc.gui.frontend.client.search;

import java.util.HashSet;
import java.util.Set;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIResult;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.services.SearchService;

/**
 * Collector for all searches
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class Search {
	private static Search instance;

	private GUIDocument[] lastResult = new GUIDocument[0];

	private GUISearchOptions options = new GUISearchOptions();

	private Set<SearchObserver> observers = new HashSet<SearchObserver>();

	private long time;

	private boolean hasMore = false;

	private String suggestion;

	private long estimatedHits;

	private Search() {
	}

	public static Search get() {
		if (instance == null)
			instance = new Search();
		return instance;
	}

	public void addObserver(SearchObserver observer) {
		if (!observers.contains(observer))
			observers.add(observer);
	}

	public void removeObserver(SearchObserver observer) {
		if (observers.contains(observer))
			observers.remove(observer);
	}

	public GUISearchOptions getOptions() {
		return options;
	}

	public void setOptions(GUISearchOptions options) {
		this.options = options;
		for (SearchObserver observer : observers) {
			observer.onOptionsChanged(options);
		}
	}

	public void search() {
		ContactingServer.get().show();

		SearchService.Instance.get().search(options, new AsyncCallback<GUIResult>() {

			@Override
			public void onFailure(Throwable caught) {
				ContactingServer.get().hide();
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIResult result) {
				try {
					suggestion = result.getSuggestion();
					time = result.getTime();
					estimatedHits = result.getEstimatedHits();
					hasMore = result.isHasMore();
					lastResult = result.getHits();

					for (SearchObserver observer : observers)
						observer.onSearchArrived();
				} finally {
					ContactingServer.get().hide();
				}
			}
		});
	}

	public GUIDocument[] getLastResult() {
		return lastResult;
	}

	public long getTime() {
		return time;
	}

	public boolean isEmpty() {
		return (getLastResult() == null || getLastResult().length == 0);
	}

	public boolean isHasMore() {
		return hasMore;
	}

	public void setHasMore(boolean hasMore) {
		this.hasMore = hasMore;
	}

	public String getSuggestion() {
		return suggestion;
	}

	public long getEstimatedHits() {
		return estimatedHits;
	}

	public void setEstimatedResults(long estimatedResults) {
		this.estimatedHits = estimatedResults;
	}
}