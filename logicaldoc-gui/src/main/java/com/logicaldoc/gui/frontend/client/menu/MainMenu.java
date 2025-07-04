package com.logicaldoc.gui.frontend.client.menu;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.google.gwt.core.client.GWT;
import com.google.gwt.http.client.URL;
import com.google.gwt.user.client.Window;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.controllers.DocumentObserver;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.controllers.FolderObserver;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.SecurityUtil;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.ValuesCallback;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.ai.robot.RobotTray;
import com.logicaldoc.gui.frontend.client.chatgpt.ChatGPTTray;
import com.logicaldoc.gui.frontend.client.docusign.DocuSignSettings;
import com.logicaldoc.gui.frontend.client.docusign.EnvelopeDetails;
import com.logicaldoc.gui.frontend.client.docusign.Envelopes;
import com.logicaldoc.gui.frontend.client.dropbox.DropboxAuthorization;
import com.logicaldoc.gui.frontend.client.dropbox.DropboxDialog;
import com.logicaldoc.gui.frontend.client.dropbox.DropboxService;
import com.logicaldoc.gui.frontend.client.google.drive.DriveMenuItem;
import com.logicaldoc.gui.frontend.client.menu.features.Features;
import com.logicaldoc.gui.frontend.client.onlyoffice.OnlyOfficeCreate;
import com.logicaldoc.gui.frontend.client.onlyoffice.OnlyOfficeEditor;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.services.ChatGPTService;
import com.logicaldoc.gui.frontend.client.services.DocuSignService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.logicaldoc.gui.frontend.client.services.ShareFileService;
import com.logicaldoc.gui.frontend.client.services.TenantService;
import com.logicaldoc.gui.frontend.client.sharefile.ShareFileDialog;
import com.logicaldoc.gui.frontend.client.sharefile.ShareFileSettings;
import com.logicaldoc.gui.frontend.client.textcontent.TextContentCreate;
import com.logicaldoc.gui.frontend.client.textcontent.TextContentEditor;
import com.logicaldoc.gui.frontend.client.webcontent.WebcontentCreate;
import com.logicaldoc.gui.frontend.client.webcontent.WebcontentEditor;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Main program menu
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MainMenu extends ToolStrip implements FolderObserver, DocumentObserver {

	private static final String CREATEDOC = "createdoc";

	private static final String EDITDOC = "editdoc";

	private static final String APIKEY = "apikey";

	private static final String AUTHORIZE = "authorize";

	private static final String WINDOW_SETTNGS = "location=no,status=no,toolbar=no,menubar=no,resizable=yes,scrollbars=yes";

	private static final String BLANK = "_blank";

	private ToolStripButton tools;

	private static MainMenu instance = null;

	private ToolStripButton evaluation;

	private List<MenuTray> trays = new ArrayList<>();

	private int currentTray = 0;

	public static MainMenu get() {
		if (instance == null)
			instance = new MainMenu();
		return instance;
	}

	private MainMenu() {
		super();

		FolderController.get().addObserver(this);
		DocumentController.get().addObserver(this);

		setWidth100();

		trays.add(new QuickSearchTray());
		if (Feature.enabled(Feature.ROBOT)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.ROBOTS))
			trays.add(new RobotTray());
		if (Feature.enabled(Feature.CHATGPT)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.CHATGPT))
			trays.add(new ChatGPTTray());

		boolean banner = Session.get().getConfigAsBoolean("gui.banner");

		if (!banner) {
			addLogo();
		} else {
			addSpacer(10);
		}

		addAccountButton();
		addToolsButton(FolderController.get().getCurrentFolder(), null);

		if (com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.INTERFACE_DENSITY)) {
			addSeparator();
			addFormItem(getDensitySelector());
		}

		if (Feature.enabled(Feature.MULTI_TENANT) && Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN)
				&& Session.get().getUser().getTenant().getTenantId() == Constants.TENANT_DEFAULTID) {
			SelectItem tenantItem = ItemFactory.newTenantSelector();
			tenantItem.setShowTitle(false);
			tenantItem.setValue(Long.toString(Session.get().getInfo().getTenant().getId()));
			tenantItem.addChangedHandler(event -> {
				long tenantId = Long.parseLong(event.getValue().toString());
				if (tenantId != Session.get().getInfo().getTenant().getId())
					TenantService.Instance.get().changeSessionTenant(tenantId, new DefaultAsyncCallback<>() {

						@Override
						public void onSuccess(GUITenant tenant) {
							Session.get().getInfo().setTenant(tenant);
							Util.redirectToRoot();
						}
					});
			});
			addFormItem(tenantItem);
		}

		addSeparator();
		addActivableFeaturesButton();
		addProductEvaluationButton();
		addSeparator();
		addSupportButton();
		addLogoutButton();

		addFill();

		addTrayBar();

		onFolderSelected(FolderController.get().getCurrentFolder());
	}

	/**
	 * Adds the logo image into the main menu
	 */
	private void addLogo() {
		Img logoImage = ItemFactory.newBrandImg("logo_menu.png", Session.get().getInfo());
		logoImage.setStyleName("logo_head");
		logoImage.setWidth(154);
		logoImage.setHeight(30);
		logoImage.setTooltip(Session.get().getInfo().getBranding().getProductName());
		addMember(logoImage);
	}

	private void addAccountButton() {
		ToolStripButton account = AwesomeFactory.newToolStripButton("user", I18N.message("loggedasintotenant",
				Session.get().getUser().getUsername(), Session.get().getInfo().getTenant().getDisplayName()),
				I18N.message("account"));
		account.addClickHandler(event -> new AccountMenu().showContextMenu());
		addButton(account, 1);
	}

	/**
	 * Adds a tray bar that displays one of the registered trays
	 */
	private void addTrayBar() {
		HLayout trayPanel = new HLayout();
		trayPanel.setAutoWidth();
		trayPanel.addMember(trays.get(0));
		addMember(trayPanel);

		ToolStripButton rotateTrays = AwesomeFactory.newToolStripButton("exchange-alt", null, null);
		rotateTrays.addClickHandler(click -> {
			if (currentTray >= trays.size() - 1)
				currentTray = 0;
			else
				currentTray++;
			trayPanel.removeMembers(trayPanel.getMembers());
			trayPanel.addMember(trays.get(currentTray));
		});
		if (trays.size() > 1)
			addButton(rotateTrays);
	}

	private MenuItem getWebContentMenuItem(GUIFolder folder, final GUIDocument document) {
		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		final MenuItem edit = new MenuItem(I18N.message(EDITDOC));
		edit.addClickHandler(event -> {
			if (document == null)
				return;

			if (document.getStatus() == 0) {
				// Need to checkout first
				DocumentService.Instance.get().checkout(Arrays.asList(document.getId()), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						DocUtil.markCheckedOut(document);
						WebcontentEditor popup = new WebcontentEditor(document);
						popup.show();
					}
				});
			} else {
				SC.warn(I18N.message("event.locked"));
			}
		});

		final MenuItem create = new MenuItem(I18N.message(CREATEDOC));
		create.addClickHandler(event -> new WebcontentCreate().show());

		menu.setItems(edit, create);
		edit.setEnabled(document != null && document.getImmutable() == 0 && folder != null && folder.isDownload()
				&& folder.isWrite() && Util.isWebContentFile(document.getFileName())
				&& Feature.enabled(Feature.WEBCONTENT));
		create.setEnabled(folder != null && folder.isDownload() && folder.isWrite()
				&& Feature.enabled(Feature.WEBCONTENT) && MainPanel.get().isOnDocumentsTab());

		MenuItem webcontentItems = new MenuItem(I18N.message("webcontent"));
		webcontentItems.setSubmenu(menu);

		return webcontentItems;
	}

	private MenuItem getTextContentMenuItem(GUIFolder folder, final GUIDocument document) {
		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		final MenuItem edit = new MenuItem(I18N.message(EDITDOC));
		edit.addClickHandler(event -> {
			if (document == null)
				return;

			if (document.getStatus() == 0) {
				// Need to checkout first
				DocumentService.Instance.get().checkout(Arrays.asList(document.getId()), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						DocUtil.markCheckedOut(document);
						TextContentEditor popup = new TextContentEditor(document, null);
						popup.show();
					}
				});
			} else {
				SC.warn(I18N.message("event.locked"));
			}
		});

		final MenuItem create = new MenuItem(I18N.message(CREATEDOC));
		create.addClickHandler((MenuItemClickEvent event) -> new TextContentCreate().show());

		menu.setItems(edit, create);
		edit.setEnabled(document != null && document.getImmutable() == 0 && folder != null && folder.isDownload()
				&& folder.isWrite() && Util.isTextFile(document.getFileName()));
		create.setEnabled(
				folder != null && folder.isDownload() && folder.isWrite() && MainPanel.get().isOnDocumentsTab());

		MenuItem textcontentItems = new MenuItem(I18N.message("textcontent"));
		textcontentItems.setSubmenu(menu);

		return textcontentItems;
	}

	private MenuItem getDropboxMenuItem(GUIFolder folder) {
		final MenuItem exportTo = new MenuItem(I18N.message("exporttodropbox"));
		exportTo.addClickHandler(event -> DropboxService.Instance.get().isConnected(new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Boolean connected) {
				if (Boolean.FALSE.equals(connected))
					DropboxAuthorization.get().show();
				else
					new DropboxDialog(true).show();
			}
		}));

		final MenuItem importFrom = new MenuItem(I18N.message("importfromdropbox"));
		importFrom.addClickHandler(
				event -> DropboxService.Instance.get().isConnected(new DefaultAsyncCallback<Boolean>() {
					@Override
					public void onSuccess(Boolean connected) {
						if (Boolean.FALSE.equals(connected))
							DropboxAuthorization.get().show();
						else
							new DropboxDialog(false).show();
					}
				}));

		final MenuItem authrorize = new MenuItem(I18N.message(AUTHORIZE));
		authrorize.addClickHandler(event -> DropboxAuthorization.get().show());

		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);
		menu.setItems(authrorize, exportTo, importFrom);

		exportTo.setEnabled(folder != null && folder.isDownload() && Feature.enabled(Feature.DROPBOX));
		importFrom.setEnabled(folder != null && folder.isWrite() && Feature.enabled(Feature.DROPBOX)
				&& MainPanel.get().isOnDocumentsTab());

		MenuItem dropboxItem = new MenuItem(I18N.message("dropbox"));
		dropboxItem.setSubmenu(menu);

		return dropboxItem;
	}

	private MenuItem getChatGPTMenuItem() {
		final MenuItem settings = new MenuItem(I18N.message("settings"));
		settings.addClickHandler(click -> ChatGPTService.Instance.get().loadSettings(new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(List<GUIValue> settings) {
				TextItem apiKey = ItemFactory.newPasswordItem(APIKEY, APIKEY, GUIValue.getValue(APIKEY, settings));
				apiKey.setWidth(360);

				TextItem model = ItemFactory.newTextItem("model", GUIValue.getValue("model", settings));

				LD.askForValues(I18N.message("chatgpt"), null, Arrays.asList(apiKey, model), 400, new ValuesCallback() {
					@Override
					public void execute(Map<String, Object> values) {
						List<GUIValue> settings = new ArrayList<>();
						for (Map.Entry<String, Object> val : values.entrySet())
							settings.add(new GUIValue(val.getKey(), "" + val.getValue()));

						ChatGPTService.Instance.get().saveSettings(settings, new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(Void arg0) {
								// Nothing to do
							}
						});
					}

					@Override
					public void execute(String value) {
						// Ignore
					}
				});
			}
		})

		);

		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);
		menu.setItems(settings);

		MenuItem chatgptItem = new MenuItem(I18N.message("chatgpt"));
		chatgptItem.setSubmenu(menu);

		return chatgptItem;
	}

	private MenuItem getShareFileMenuItem(GUIFolder folder) {
		final MenuItem exportTo = new MenuItem(I18N.message("exporttosharefile"));
		exportTo.addClickHandler((MenuItemClickEvent event) -> new ShareFileDialog(true).show());

		final MenuItem importFrom = new MenuItem(I18N.message("importfromsharefile"));
		importFrom.addClickHandler(event -> ShareFileService.Instance.get().isAuthorized(new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Boolean authorized) {
				LD.clearPrompt();
				if (Boolean.TRUE.equals(authorized)) {
					ShareFileDialog dialog = new ShareFileDialog(false);
					dialog.show();
				} else {
					SC.say(I18N.message("youneedtoauthorizesharefile",
							Session.get().getInfo().getBranding().getProduct()),
							(Boolean value) -> new ShareFileSettings().show());
				}
			}
		}));

		final MenuItem authorize = new MenuItem(I18N.message(AUTHORIZE));
		authorize.addClickHandler(event -> new ShareFileSettings().show());

		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);
		menu.setItems(authorize, exportTo, importFrom);

		exportTo.setEnabled(folder != null && folder.isDownload() && Feature.enabled(Feature.SHAREFILE));
		importFrom.setEnabled(folder != null && folder.isWrite() && Feature.enabled(Feature.SHAREFILE)
				&& MainPanel.get().isOnDocumentsTab());

		MenuItem sharefileItem = new MenuItem(I18N.message("sharefile"));
		sharefileItem.setSubmenu(menu);

		return sharefileItem;
	}

	private MenuItem getDocuSignMenuItem(GUIDocument document) {
		final MenuItem authorize = new MenuItem(I18N.message(AUTHORIZE));
		authorize.addClickHandler(click -> new DocuSignSettings().show());

		final MenuItem sendEnvelope = new MenuItem(I18N.message("sendenvelope"));
		sendEnvelope.addClickHandler(event -> {
			LD.contactingServer();
			DocuSignService.Instance.get().isAuthorized(new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(Boolean authorized) {
					LD.clearPrompt();
					if (Boolean.TRUE.equals(authorized)) {
						new EnvelopeDetails().show();
					} else {
						SC.say(I18N.message("youneedtoauthorizedocusign",
								Session.get().getInfo().getBranding().getProduct()),
								(Boolean value) -> new DocuSignSettings().show());
					}
				}
			});
		});
		sendEnvelope.setEnabled(document != null);

		final MenuItem envelopes = new MenuItem(I18N.message("envelopes"));
		envelopes.addClickHandler(event -> {
			LD.contactingServer();
			DocuSignService.Instance.get().isAuthorized(new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(Boolean authorized) {
					LD.clearPrompt();
					if (Boolean.TRUE.equals(authorized)) {
						new Envelopes().show();
					} else {
						SC.say(I18N.message("youneedtoauthorizedocusign",
								Session.get().getInfo().getBranding().getProduct()),
								(Boolean value) -> new DocuSignSettings().show());
					}
				}
			});
		});

		sendEnvelope.setEnabled(Feature.enabled(Feature.DOCUSIGN));
		envelopes.setEnabled(Feature.enabled(Feature.DOCUSIGN));
		authorize.setEnabled(Feature.enabled(Feature.DOCUSIGN));

		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);
		menu.setItems(authorize, sendEnvelope, envelopes);

		MenuItem docuSignItem = new MenuItem(I18N.message("docusign"));
		docuSignItem.setSubmenu(menu);

		return docuSignItem;
	}

	private MenuItem getOfficeMenuItem(GUIFolder folder, final GUIDocument document) {
		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		final MenuItem edit = new MenuItem(I18N.message("editwithoffice"));
		edit.addClickHandler(event -> {
			if (document == null)
				return;

			WindowUtils.openUrl("ldedit:" + GWT.getHostPageBaseURL() + "ldedit?action=edit&sid="
					+ Session.get().getSid() + "&docId=" + document.getId());
		});

		menu.setItems(edit);

		edit.setEnabled(document != null && document.getImmutable() == 0 && folder != null && folder.isDownload()
				&& folder.isWrite() && Feature.enabled(Feature.OFFICE));

		MenuItem officeItem = new MenuItem(I18N.message("microsoftoffice"));
		officeItem.setSubmenu(menu);

		return officeItem;
	}

	private MenuItem getOnlyOfficeMenuItem(GUIFolder folder, final GUIDocument document) {
		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		final MenuItem edit = new MenuItem(I18N.message(EDITDOC));
		edit.addClickHandler(event -> {
			if (document == null)
				return;
			SecurityUtil.checkPermissionsAndRun(Arrays.asList(document.getId()),
					new String[] { GUIAccessControlEntry.PERMISSION_DOWNLOAD, GUIAccessControlEntry.PERMISSION_WRITE },
					() -> new OnlyOfficeEditor(document).show());
		});

		final MenuItem create = new MenuItem(I18N.message(CREATEDOC));
		create.addClickHandler(event -> new OnlyOfficeCreate().show());

		// This should be enabled only on PDF
		final MenuItem fillForms = new MenuItem("Fill in PDF forms");
		fillForms.addClickHandler(click -> {
			if (document == null)
				return;

			// Setup something in the document that we will check later in the
			// editor
			document.setComment("fillForms");
			new OnlyOfficeEditor(document).show();
		});

		menu.setItems(edit, create, fillForms);

		edit.setEnabled(document != null && document.getImmutable() == 0 && folder != null && folder.isDownload()
				&& folder.isWrite());

		create.setEnabled(folder != null && folder.isDownload() && folder.isWrite());

		// enabled only on PDF
		fillForms.setEnabled(document != null && document.getImmutable() == 0 && folder != null && folder.isDownload()
				&& folder.isWrite() && (document.getType().toLowerCase().endsWith("pdf")));

		MenuItem onlyOfficeItem = new MenuItem(I18N.message("onlyoffice"));
		onlyOfficeItem.setSubmenu(menu);

		return onlyOfficeItem;
	}

	private void addToolsButton(GUIFolder folder, GUIDocument document) {
		tools = AwesomeFactory.newToolStripButton("toolbox", I18N.message("tools"), I18N.message("tools"));
		tools.addClickHandler(click -> buildToolsMenu(folder, document).showContextMenu());

		addButton(tools, 2);
	}

	public Menu buildToolsMenu(GUIFolder folder, GUIDocument document) {
		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		if (folder == null && document != null)
			folder = document.getFolder();

		MenuItem develConsole = new MenuItem(I18N.message("develconsole"));
		develConsole.addClickHandler(click -> SC.showConsole());

		if (document != null || folder != null)
			addItemsWhenFolderOrDocumentSelected(folder, document, menu);

		menu.addItem(getSupportMenuItem());

		addRegistration(menu, develConsole);
		return menu;
	}

	private void addItemsWhenFolderOrDocumentSelected(GUIFolder folder, GUIDocument document, Menu menu) {
		if (Feature.enabled(Feature.DROPBOX)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.DROPBOX))
			menu.addItem(getDropboxMenuItem(folder));
		if (Feature.enabled(Feature.SHAREFILE)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.SHAREFILE))
			menu.addItem(getShareFileMenuItem(folder));
		if (Feature.enabled(Feature.GOOGLE_DRIVE)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.GOOGLEDRIVE))
			menu.addItem(new DriveMenuItem(folder, document));
		if (Feature.enabled(Feature.ONLYOFFICE)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.ONLYOFFICE)
				&& Session.get().getConfigAsBoolean("converter.OnlyOfficeConverter.enabled"))
			menu.addItem(getOnlyOfficeMenuItem(folder, document));
		if (Feature.enabled(Feature.DOCUSIGN)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.DOCUSIGN))
			menu.addItem(getDocuSignMenuItem(document));
		if (Feature.enabled(Feature.OFFICE)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.OFFICE))
			menu.addItem(getOfficeMenuItem(folder, document));
		addTextAndWebContentItems(menu, document, folder);
		addChatGPTItem(menu);
	}

	private void addTextAndWebContentItems(Menu menu, GUIDocument document, GUIFolder folder) {
		if (Feature.enabled(Feature.WEBCONTENT)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.WEBCONTENT))
			menu.addItem(getWebContentMenuItem(folder, document));
		if (com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.TEXTCONTENT))
			menu.addItem(getTextContentMenuItem(folder, document));
	}

	private void addChatGPTItem(Menu menu) {
		if (Feature.enabled(Feature.CHATGPT)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.CHATGPT))
			menu.addItem(getChatGPTMenuItem());
	}

	private void addRegistration(Menu menu, MenuItem develConsole) {
		if (Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN)) {
			MenuItem registration = new MenuItem(I18N.message("registration"));
			registration.addClickHandler(registrationClick -> SettingService.Instance.get().loadSettingsByNames(
					Arrays.asList("reg.name", "reg.email", "reg.organization", "reg.website"),
					new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(List<GUIParameter> reg) {
							new Registration(reg.stream().map(r -> r.getValue()).collect(Collectors.toList())).show();
						}
					}));
			menu.addItem(registration);

			if (Session.get().isDevel()) {
				menu.addItem(develConsole);
			}
		}
	}

	private SelectItem getDensitySelector() {
		SelectItem density = ItemFactory.newDensitySelector();
		density.setShowTitle(false);
		String dens = Session.get().getInfo().getConfig("gui.density");
		if (CookiesManager.get(CookiesManager.COOKIE_DENSITY) != null)
			dens = CookiesManager.get(CookiesManager.COOKIE_DENSITY);
		density.setValue(dens);
		density.addChangedHandler((ChangedEvent event) -> {
			String newDensity = event.getValue().toString();
			Session.get().getInfo().setConfig(Session.get().getTenantName() + ".gui.density", newDensity);
			CookiesManager.save(CookiesManager.COOKIE_DENSITY, newDensity);
			Util.setupDensity(Session.get().getInfo());
			Util.redirectToRoot();
		});
		return density;
	}

	private void addActivableFeaturesButton() {
		ToolStripButton activable = AwesomeFactory.newToolStripButton("lightbulb-on", I18N.message("activablefeatures"),
				I18N.message("activablefeatures"));
		activable.addClickHandler(event -> Features.get().show());

		if (!Session.get().isDemo()
				&& Session.get().getInfo().getBranding().getUrl().equals("https://www.logicaldoc.com")
				&& Feature.enabled(Feature.OFFICE) && com.logicaldoc.gui.common.client.Menu
						.enabled(com.logicaldoc.gui.common.client.Menu.ACTIVABLE_FEATURES))
			addButton(activable);
	}

	private void addProductEvaluationButton() {
		evaluation = AwesomeFactory.newToolStripButton("star", I18N.message("productevaluation"),
				I18N.message("productevaluation"));
		evaluation.addClickHandler(event -> {
			String url = Session.get().getInfo().getBranding().getEvaluation();
			url = url.replace("NAME", URL.encodeQueryString(Session.get().getUser().getFullName()));
			url = url.replace("COMPANY",
					Session.get().getUser().getCompany() != null && !Session.get().getUser().getCompany().isEmpty()
							? URL.encodeQueryString(Session.get().getUser().getCompany())
							: URL.encodeQueryString(Session.get().getInfo().getLicensee()));
			url = url.replace("PRODUCT", URL.encodeQueryString(Session.get().getInfo().getBranding().getProductName()));
			WindowUtils.openUrl(url, BLANK);
		});
		addButton(evaluation);
		refreshProductEvaluationButton();
	}

	public void refreshProductEvaluationButton() {
		boolean mustShow = !Session.get().isDemo() && !Session.get().getInfo().getBranding().getEvaluation().isEmpty()
				&& Session.get().getUser().isEvalFormEnabled() && Feature.enabled(Feature.OFFICE)
				&& com.logicaldoc.gui.common.client.Menu
						.enabled(com.logicaldoc.gui.common.client.Menu.PRODUCT_EVALUATION);
		evaluation.setVisible(mustShow);
	}

	private void addSupportButton() {
		Menu menu = buildSupportMenu();
		ToolStripButton supportButton = AwesomeFactory.newToolStripButton("question-circle", "support");
		supportButton.addClickHandler((com.smartgwt.client.widgets.events.ClickEvent event) -> menu.showContextMenu());
		addButton(supportButton);
	}

	/**
	 * Creates the Support menu item.
	 * 
	 * @return the support menu item
	 */
	private MenuItem getSupportMenuItem() {
		Menu menu = buildSupportMenu();
		MenuItem supportItem = new MenuItem(I18N.message("support"));
		supportItem.setSubmenu(menu);
		return supportItem;
	}

	/**
	 * Builds the support menu
	 * 
	 * @return The support menu
	 */
	private Menu buildSupportMenu() {
		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		MenuItem manuals = new MenuItem(I18N.message("manuals"));
		manuals.addClickHandler((MenuItemClickEvent event) -> Window.open(
				Session.get().getInfo().getBranding().getHelp() + "?lang=" + I18N.getLocale(), BLANK, WINDOW_SETTNGS));
		menu.addItem(manuals);

		MenuItem bugReport = new MenuItem(I18N.message("bug.report"));
		bugReport.addClickHandler((MenuItemClickEvent event) -> Window
				.open(Session.get().getInfo().getBranding().getBugs(), BLANK, WINDOW_SETTNGS));
		if (Session.get().getInfo().getBranding().getBugs() != null
				&& !"-".equals(Session.get().getInfo().getBranding().getBugs())
				&& Feature.enabled(Feature.TECHNICAL_SUPPORT))
			menu.addItem(bugReport);

		MenuItem forum = new MenuItem(I18N.message("forum"));
		forum.addClickHandler((MenuItemClickEvent event) -> Window
				.open(Session.get().getInfo().getBranding().getForum(), BLANK, WINDOW_SETTNGS));
		if (Session.get().getInfo().getBranding().getForum() != null
				&& !"-".equals(Session.get().getInfo().getBranding().getForum()))
			menu.addItem(forum);

		MenuItem about = new MenuItem(I18N.message("about") + " " + Session.get().getInfo().getBranding().getProduct());
		about.addClickHandler(event -> new AboutDialog().show());
		menu.addItem(about);
		return menu;
	}

	private void addLogoutButton() {
		ToolStripButton logoutButton = AwesomeFactory.newToolStripButton("power-off", "logout");
		logoutButton.addClickHandler(event -> Session.get().logout());
		addButton(logoutButton);
	}

	@Override
	public void onFolderSelected(GUIFolder folder) {
		updateToolsMenu(folder, null);
	}

	@Override
	public void onDocumentSelected(GUIDocument document) {
		updateToolsMenu(document.getFolder(), document);
	}

	@Override
	public void onFolderChanged(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onDocumentModified(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onFolderDeleted(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderCreated(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderMoved(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderBeginEditing(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onFolderCancelEditing(GUIFolder folder) {
		// Nothing to do
	}

	@Override
	public void onDocumentsDeleted(List<GUIDocument> documents) {
		// Nothing to do
	}

	@Override
	public void onDocumentMoved(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentCheckedIn(GUIDocument document) {
		onDocumentModified(document);
	}

	@Override
	public void onDocumentCheckedOut(GUIDocument document) {
		onDocumentModified(document);
	}

	@Override
	public void onDocumentLocked(GUIDocument document) {
		onDocumentModified(document);
	}

	@Override
	public void onDocumentUnlocked(GUIDocument document) {
		onDocumentModified(document);
	}

	@Override
	public void onDocumentStored(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentBeginEditing(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentCancelEditing(GUIDocument document) {
		// Nothing to do
	}

	/**
	 * Invoked when a tab of the main panel is selected
	 * 
	 * @param tab name of the selected tab
	 */
	public void onTabSeleted(String tab) {
		if ("documents".equals(tab)) {
			onFolderSelected(FolderController.get().getCurrentFolder());
		} else {
			updateToolsMenu(null, null);
		}
	}

	@Override
	public void destroy() {
		FolderController.get().removeObserver(this);
		DocumentController.get().removeObserver(this);
	}

	@Override
	protected void onUnload() {
		destroy();
		super.onUnload();
	}

	@Override
	protected void onDestroy() {
		destroy();
		super.onDestroy();
	}

	private void updateToolsMenu(GUIFolder folder, GUIDocument document) {
		if (tools != null)
			removeMember(tools);

		addToolsButton(folder, document);
		addMember(tools, 2);
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}