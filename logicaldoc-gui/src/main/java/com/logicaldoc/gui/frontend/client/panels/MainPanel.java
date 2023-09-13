package com.logicaldoc.gui.frontend.client.panels;

import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.SessionObserver;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.services.InfoService;
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
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.tab.events.TabSelectedHandler;

/**
 * This is the main panel that collects all other GUI panels
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MainPanel extends VLayout implements SessionObserver {

	private static final String SEARCH = "search";

	private static final String DOCUMENT = "documents";

	private TabSet tabSet = new TabSet();

	private Tab documentsTab;

	private Tab searchTab;

	private Tab dashboardTab;

	private Tab administrationTab;

	private static MainPanel instance;

	private IncomingMessage incomingMessage = null;

	public static MainPanel get() {
		if (instance == null)
			instance = new MainPanel();
		return instance;
	}

	private MainPanel() {
		setOverflow(Overflow.HIDDEN);
	}

	@Override
	public void onDraw() {
		Session.get().addObserver(this);

		setWidth100();
		setHeight100();

		documentsTab = new Tab(I18N.message(DOCUMENT));
		documentsTab.setName(DOCUMENT);
		searchTab = new Tab(I18N.message(SEARCH));
		searchTab.setName(SEARCH);
		dashboardTab = new Tab(I18N.message("dashboard"));
		dashboardTab.setName("dashboard");
		administrationTab = new Tab(I18N.message("administration"));
		administrationTab.setName("administration");

		Window.addResizeHandler(event -> {
			int width = Window.getClientWidth();
			int height = Window.getClientHeight();
			tabSet.setSize(width + "px", height - 95 + "px");
			redraw();
		});

		/*
		 * Setup notification logic
		 */
		TabSelectedHandler selectionHandler = event -> MainMenu.get().onTabSeleted(event.getTab().getName());

		documentsTab.addTabSelectedHandler(selectionHandler);
		searchTab.addTabSelectedHandler(selectionHandler);
		dashboardTab.addTabSelectedHandler(selectionHandler);
		administrationTab.addTabSelectedHandler(selectionHandler);
	}

	@Override
	public void onUserLoggedIn(final GUIUser user) {
		prepareIncomingMessage();

		setMembers(new TopPanel(), incomingMessage, MainMenu.get(), tabSet, new StatusBar(true));

		prepareMainTabs(user);

		/*
		 * Check if there are alerts
		 */
		if (user.isMemberOf(Constants.GROUP_ADMIN) && !Session.get().isDemo())
			retrieveAlerts();
	}

	private void retrieveAlerts() {
		InfoService.Instance.get().getInfo(I18N.getLocale(), Session.get().getTenantName(), false,
				new AsyncCallback<GUIInfo>() {
					@Override
					public void onFailure(Throwable caught) {
						// Nothing to do
					}

					@Override
					public void onSuccess(GUIInfo info) {
						StringBuilder alerts = new StringBuilder();
						for (GUIMessage warning : info.getAlerts()) {
							if (warning.getPriority() == GUIMessage.PRIO_WARN && warning.isShowInGUI()) {
								if (!"".equals(alerts.toString()))
									alerts.append(" -- ");
								alerts.append(warning.getMessage());
							}
						}
						if (!"".equals(alerts.toString()))
							SC.warn(alerts.toString());
					}
				});
	}

	private void prepareMainTabs(final GUIUser user) {
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

		openDefaultTab(welcomeScreen);
	}

	private void openDefaultTab(long welcomeScreen) {
		RequestInfo loc = WindowUtils.getRequestInfo();
		if ((loc.getParameter("folderId") != null || loc.getParameter("docId") != null)
				&& Menu.enabled(Menu.DOCUMENTS)) {
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
	}

	private void prepareIncomingMessage() {
		incomingMessage = new IncomingMessage(Session.get().getIncomingMessage(),
				event -> MainPanel.this.getIncomingMessage().setVisible(false));
		incomingMessage.setVisible(
				Session.get().getIncomingMessage() != null && !Session.get().getIncomingMessage().isEmpty());
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
		selectDashboardTab();
		DashboardPanel.get().setDefaultOpenTab(DashboardPanel.USER_ID);
		DashboardPanel.get().getTabSet().selectTab(DashboardPanel.USER_ID);
	}

	public void selectWorkflowTab() {
		selectDashboardTab();
		DashboardPanel.get().setDefaultOpenTab(DashboardPanel.WORKFLOW);
		DashboardPanel.get().getTabSet().selectTab(DashboardPanel.WORKFLOW);
	}

	public void selectMessagesTab() {
		selectDashboardTab();
		DashboardPanel.get().setDefaultOpenTab(DashboardPanel.MESSAGES);
		DashboardPanel.get().getTabSet().selectTab(DashboardPanel.MESSAGES);
	}

	public void selectCalendarTab() {
		selectDashboardTab();
		DashboardPanel.get().setDefaultOpenTab(DashboardPanel.CALENDAR);
		DashboardPanel.get().getTabSet().selectTab(DashboardPanel.CALENDAR);
	}
	
	public void selectReadingsTab() {
		selectDashboardTab();
		DashboardPanel.get().setDefaultOpenTab(DashboardPanel.READINGREQUESTS);
		DashboardPanel.get().getTabSet().selectTab(DashboardPanel.READINGREQUESTS);
	}

	public boolean isOnDocumentsTab() {
		return DOCUMENT.equals(tabSet.getSelectedTab().getName());
	}

	public boolean isOnSearchTab() {
		return SEARCH.equals(tabSet.getSelectedTab().getName());
	}

	public IncomingMessage getIncomingMessage() {
		return incomingMessage;
	}
}