package com.logicaldoc.gui.frontend.client.search;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIResult;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentGridUtil;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.services.SearchService;

/**
 * Collector for all searches
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class Search {
	private static Search instance;

	private List<GUIDocument> lastResult = new ArrayList<>();

	private GUISearchOptions options = new GUISearchOptions();

	private Set<SearchObserver> observers = new HashSet<>();

	private long time;

	private boolean hasMore = false;

	private long estimatedHits;

	private Integer maxHits;

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
		LD.contactingServer();

		SearchService.Instance.get().search(options, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(GUIResult result) {
				try {
					time = result.getTime();
					estimatedHits = result.getEstimatedHits();
					hasMore = result.isHasMore();
					lastResult = result.getHits();
					for (SearchObserver observer : observers)
						observer.onSearchArrived();
				} finally {
					LD.clearPrompt();
					SearchPanel.get().onDraw();
					MainPanel.get().selectSearchTab();
				}
			}
		});
	}

	public List<GUIDocument> getLastResult() {
		return lastResult;
	}

	public long getTime() {
		return time;
	}

	public boolean isEmpty() {
		return getLastResult().isEmpty();
	}

	public boolean isHasMore() {
		return hasMore;
	}

	public void setHasMore(boolean hasMore) {
		this.hasMore = hasMore;
	}

	public long getEstimatedHits() {
		return estimatedHits;
	}

	public void setEstimatedResults(long estimatedResults) {
		this.estimatedHits = estimatedResults;
	}

	public int getMaxHits() {
		if (maxHits == null) {
			Integer pageSize = DocumentGridUtil.getPageSizeFromSpec(Session.get().getUser().getHitsGrid());
			if (pageSize == null)
				pageSize = Session.get().getConfigAsInt("search.hits");
			maxHits = pageSize;
		}
		return maxHits;
	}

	public void setMaxHits(int maxHits) {
		this.maxHits = maxHits;
	}
}