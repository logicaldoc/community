package com.logicaldoc.gui.frontend.client.panels;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.event.logical.shared.ResizeEvent;
import com.google.gwt.event.logical.shared.ResizeHandler;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.datepicker.client.CalendarUtil;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.PanelObserver;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.SessionObserver;
import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.RequestInfo;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.IncomingMessage;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.dashboard.DashboardPanel;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.menu.MainMenu;
import com.logicaldoc.gui.frontend.client.search.Search;
import com.logicaldoc.gui.frontend.client.search.SearchMenu;
import com.logicaldoc.gui.frontend.client.search.SearchPanel;
import com.logicaldoc.gui.frontend.client.services.CalendarService;
import com.logicaldoc.gui.frontend.client.services.CalendarServiceAsync;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.logicaldoc.gui.frontend.client.services.WorkflowServiceAsync;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.tab.events.TabSelectedEvent;
import com.smartgwt.client.widgets.tab.events.TabSelectedHandler;

/**
 * This is the main panel that collects all other GUI panels
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class MainPanel extends VLayout implements SessionObserver {

	private TabSet tabSet = new TabSet();

	private Tab documentsTab;

	private Tab searchTab;

	private Tab dashboardTab;

	private Tab administrationTab;

	private static MainPanel instance;

	private IncomingMessage incomingMessage = null;

	private List<PanelObserver> observers = new ArrayList<PanelObserver>();

	public static MainPanel get() {
		if (instance == null)
			instance = new MainPanel();
		return instance;
	}

	private MainPanel() {
		setWidth100();
		setHeight100();

		Session.get().addSessionObserver(this);
	}

	public void addObjserver(PanelObserver observer) {
		observers.add(observer);
	}

	private void initGUI() {
		Layout topPanel = new TopPanel();

		tabSet.setTabBarPosition(Side.TOP);
		tabSet.setTabBarAlign(Side.LEFT);
		tabSet.setWidth100();
		tabSet.setHeight("*");
		documentsTab = new Tab(I18N.message("documents"));
		documentsTab.setName("documents");
		searchTab = new Tab(I18N.message("search"));
		searchTab.setName("search");
		dashboardTab = new Tab(I18N.message("dashboard"));
		dashboardTab.setName("dashboard");
		administrationTab = new Tab(I18N.message("administration"));
		administrationTab.setName("administration");

		addMember(topPanel);

		incomingMessage = new IncomingMessage(Session.get().getIncomingMessage(), new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				MainPanel.this.getIncomingMessage().setVisible(false);
			}
		});

		incomingMessage.setVisible(Session.get().getIncomingMessage() != null
				&& !Session.get().getIncomingMessage().isEmpty());

		addMember(incomingMessage);
		MainMenu mainMenu = new MainMenu(true);
		addObjserver(mainMenu);
		addMember(mainMenu);
		addMember(tabSet);
		addMember(new StatusBar(true));

		Window.addResizeHandler(new ResizeHandler() {
			public void onResize(ResizeEvent event) {
				int width = Window.getClientWidth();
				int height = Window.getClientHeight();
				tabSet.setSize(width + "px", height - 95 + "px");
				redraw();
			}
		});

		/*
		 * Setup notification logic
		 */
		TabSelectedHandler selectionHandler = new TabSelectedHandler() {

			@Override
			public void onTabSelected(TabSelectedEvent event) {
				for (PanelObserver observer : observers) {
					observer.onTabSeleted(event.getTab().getName());
				}
			}
		};
		documentsTab.addTabSelectedHandler(selectionHandler);
		searchTab.addTabSelectedHandler(selectionHandler);
		dashboardTab.addTabSelectedHandler(selectionHandler);
		administrationTab.addTabSelectedHandler(selectionHandler);
	}

	@Override
	public void onUserLoggedIn(final GUIUser user) {
		initGUI();

		long welcomeScreen = Menu.DASHBOARD;
		if (user.getWelcomeScreen() != null)
			welcomeScreen = user.getWelcomeScreen().intValue();

		if (Menu.enabled(Menu.DASHBOARD)) {
			dashboardTab.setPane(DashboardPanel.get());
			tabSet.addTab(dashboardTab);
		}

		if (Menu.enabled(Menu.DOCUMENTS)) {
			documentsTab.setPane(DocumentsPanel.get());
			tabSet.addTab(documentsTab);
		}

		if (Menu.enabled(Menu.SEARCH)) {
			searchTab.setPane(SearchPanel.get());
			tabSet.addTab(searchTab);
		}

		if (Menu.enabled(Menu.ADMINISTRATION)) {
			administrationTab.setPane(AdminScreen.get());
			tabSet.addTab(administrationTab);
		}

		RequestInfo loc = WindowUtils.getRequestInfo();
		if ((loc.getParameter("folderId") != null || loc.getParameter("docId") != null) && Menu.enabled(Menu.DOCUMENTS)) {
			/*
			 * The user clicked on a permanent link so we have to open the
			 * Documents tab
			 */
			tabSet.selectTab(documentsTab);
		} else {
			if (welcomeScreen == Menu.DOCUMENTS && Menu.enabled(Menu.DOCUMENTS))
				tabSet.selectTab(documentsTab);
			else if (welcomeScreen == Menu.SEARCH && Menu.enabled(Menu.SEARCH))
				tabSet.selectTab(searchTab);
			else if (welcomeScreen == Menu.DASHBOARD && Menu.enabled(Menu.DASHBOARD))
				tabSet.selectTab(dashboardTab);
		}

		WorkflowServiceAsync service = (WorkflowServiceAsync) GWT.create(WorkflowService.class);
		service.countActiveUserTasks(user.getUserName(), new AsyncCallback<Integer>() {

			@Override
			public void onFailure(Throwable caught) {
				if (Session.get().isDevel())
					Log.serverError(caught);
			}

			@Override
			public void onSuccess(Integer result) {
				user.setActiveTasks(result);
			}
		});

		CalendarServiceAsync cservice = (CalendarServiceAsync) GWT.create(CalendarService.class);
		Date date = new Date();
		CalendarUtil.addDaysToDate(date, 1);
		cservice.countUserEvents(user.getUserName(), date, new AsyncCallback<Integer>() {

			@Override
			public void onFailure(Throwable caught) {
				if (Session.get().isDevel())
					Log.serverError(caught);
			}

			@Override
			public void onSuccess(Integer result) {
				user.setUpcomingEvents(result);
			}
		});

		if (Session.get().getInfo().getAlerts() != null && user.isMemberOf("admin")) {
			String alerts = "";
			for (GUIMessage warning : Session.get().getInfo().getAlerts()) {
				if (warning.getPriority() == GUIMessage.PRIO_WARN && warning.isShowInGUI()){
					if (!"".equals(alerts))
						alerts += " -- ";
					alerts += warning.getMessage();
				}
			}
			if (!"".equals(alerts))
				SC.warn(alerts);
		}
	}

	public void selectSearchTab() {
		tabSet.selectTab(searchTab);
		if (Search.get().getOptions().isFulltext())
			SearchMenu.get().openFulltextSection();
	}

	public void selectDocumentsTab() {
		tabSet.selectTab(documentsTab);
	}

	public void selectDashboardTab() {
		tabSet.selectTab(dashboardTab);
	}

	public void selectUserTab() {
		DashboardPanel dp = DashboardPanel.get();
		dp.getTabSet().selectTab(DashboardPanel.get().getUserTab());
		dashboardTab.setPane(dp);
		tabSet.selectTab(dashboardTab);
	}

	public void selectWorkflowTab() {
		DashboardPanel dp = DashboardPanel.get();
		dp.getTabSet().selectTab(DashboardPanel.get().getWorkflowTab());
		dashboardTab.setPane(dp);
		tabSet.selectTab(dashboardTab);
	}

	public void selectMessagesTab() {
		DashboardPanel dp = DashboardPanel.get();
		dp.getTabSet().selectTab(DashboardPanel.get().getMessagesTab());
		dashboardTab.setPane(dp);
		tabSet.selectTab(dashboardTab);
	}

	public void selectCalendarTab() {
		DashboardPanel dp = DashboardPanel.get();
		dp.getTabSet().selectTab(DashboardPanel.get().getCalendarTab());
		dashboardTab.setPane(dp);
		tabSet.selectTab(dashboardTab);
	}

	public boolean isOnDocumentsTab() {
		return "documents".equals(tabSet.getSelectedTab().getName());
	}

	public boolean isOnSearchTab() {
		return "search".equals(tabSet.getSelectedTab().getName());
	}

	public IncomingMessage getIncomingMessage() {
		return incomingMessage;
	}
}