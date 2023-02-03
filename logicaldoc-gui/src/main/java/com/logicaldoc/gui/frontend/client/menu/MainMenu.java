package com.logicaldoc.gui.frontend.client.menu;

import java.util.LinkedHashMap;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUICriterion;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.observer.DocumentObserver;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.observer.FolderObserver;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.document.grid.DocumentGridUtil;
import com.logicaldoc.gui.frontend.client.docusign.DocuSignSettings;
import com.logicaldoc.gui.frontend.client.docusign.EnvelopeDetails;
import com.logicaldoc.gui.frontend.client.docusign.Envelopes;
import com.logicaldoc.gui.frontend.client.dropbox.DropboxAuthorizationWizard;
import com.logicaldoc.gui.frontend.client.dropbox.DropboxDialog;
import com.logicaldoc.gui.frontend.client.gdrive.GDriveMenuItem;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.search.Search;
import com.logicaldoc.gui.frontend.client.services.DocuSignService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.DropboxService;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.logicaldoc.gui.frontend.client.services.ShareFileService;
import com.logicaldoc.gui.frontend.client.services.TenantService;
import com.logicaldoc.gui.frontend.client.sharefile.ShareFileDialog;
import com.logicaldoc.gui.frontend.client.sharefile.ShareFileSettings;
import com.logicaldoc.gui.frontend.client.textcontent.TextContentCreate;
import com.logicaldoc.gui.frontend.client.textcontent.TextContentEditor;
import com.logicaldoc.gui.frontend.client.webcontent.WebcontentCreate;
import com.logicaldoc.gui.frontend.client.webcontent.WebcontentEditor;
import com.logicaldoc.gui.frontend.client.zoho.ZohoMenuItem;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.ClickHandler;
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

	private ToolStripButton account;

	private ToolStripButton tools;

	private static MainMenu instance = null;

	private SelectItem searchType = new SelectItem();

	private TextItem searchBox = new TextItem();

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

		if (Feature.enabled(Feature.MULTI_TENANT)) {
			if (Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN)
					&& Session.get().getUser().getTenant().getTenantId() == Constants.TENANT_DEFAULTID) {
				SelectItem tenantItem = ItemFactory.newTenantSelector();
				tenantItem.setShowTitle(false);
				tenantItem.setValue(Long.toString(Session.get().getInfo().getTenant().getId()));
				tenantItem.addChangedHandler(new ChangedHandler() {

					@Override
					public void onChanged(ChangedEvent event) {
						long tenantId = Long.parseLong(event.getValue().toString());
						if (tenantId != Session.get().getInfo().getTenant().getId())
							TenantService.Instance.get().changeSessionTenant(tenantId, new AsyncCallback<GUITenant>() {

								@Override
								public void onSuccess(GUITenant tenant) {
									Session.get().getInfo().setTenant(tenant);
									Util.redirectToRoot();
								}

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}
							});
					}
				});
				addFormItem(tenantItem);
			}
		}

		addSeparator();
		addSupportButton();
		addLogoutButton();

		addFill();

		addSearchBox();

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
		account = AwesomeFactory.newToolStripButton("user", I18N.message("loggedasintotenant",
				Session.get().getUser().getUsername(), Session.get().getInfo().getTenant().getDisplayName()),
				I18N.message("account"));
		account.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				new AccountMenu().showContextMenu();
			}
		});

// Uncomment to show the avatar		
//		account.setTitle("<div><img class='accountIcon' src='" + Util.avatarUrl(Session.get().getUser().getId()) + "' />"
//				+ I18N.message("account") + "</div>");

		addButton(account, 1);
	}

	private void onSearch() {
		GUISearchOptions options = new GUISearchOptions();

		Integer pageSize = DocumentGridUtil.getPageSizeFromSpec(Session.get().getUser().getHitsGrid());
		if (pageSize == null)
			pageSize = Session.get().getConfigAsInt("search.hits");
		options.setMaxHits(pageSize);

		String field = searchType.getValueAsString();
		String value = searchBox.getValueAsString().trim();
		if ("fulltext".equals(field)) {
			options.setType(GUISearchOptions.TYPE_FULLTEXT);
			options.setExpression(value);
			options.setExpressionLanguage(I18N.getLocale());
			options.setType(GUISearchOptions.TYPE_FULLTEXT);
			options.setFields(Constants.FULLTEXT_DEFAULT_FIELDS);
			options.setCriteria(null);
		} else {
			options.setType(GUISearchOptions.TYPE_PARAMETRIC);
			options.setTopOperator("matchall");
			GUICriterion criterion = new GUICriterion();
			criterion.setField(field);
			criterion.setOperator("contains");
			if ("id".equals(field)) {
				criterion.setOperator("equals");
				try {
					criterion.setLongValue(Long.parseLong(value));
				} catch (Throwable t) {
					criterion.setLongValue(0L);
				}
			} else
				criterion.setStringValue(value);
			options.setCaseSensitive(0);
			options.setCriteria(new GUICriterion[] { criterion });
		}

		Search.get().setOptions(options);
		Search.get().search();
	}

	private void addSearchBox() {

		searchBox.setShowTitle(false);
		searchBox.setDefaultValue(I18N.message("search") + "...");
		searchBox.setWidth(160);
		searchBox.addKeyPressHandler(new KeyPressHandler() {
			@Override
			public void onKeyPress(KeyPressEvent event) {
				if (event.getKeyName() == null)
					return;
				if (Constants.KEY_ENTER.equals(event.getKeyName().toLowerCase())) {
					onSearch();
				}
			}
		});
		searchBox.addClickHandler(new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
				if ((I18N.message("search") + "...").equals(event.getItem().getValue())) {
					event.getItem().setValue("");
				}
			}
		});

		LinkedHashMap<String, String> valueMap = new LinkedHashMap<String, String>();
		valueMap.put("fulltext", I18N.message("fulltext"));
		valueMap.put("filename", I18N.message("filename"));
		valueMap.put("id", I18N.message("id"));
		valueMap.put("customId", I18N.message("customid"));
		searchType.setWidth(130);
		searchType.setShowTitle(false);
		searchType.setValueMap(valueMap);
		searchType.setValue("fulltext");

		addFormItem(searchBox);

		if (Feature.enabled(Feature.PARAMETRIC_SEARCHES)) {
			addFormItem(searchType);
		}

		ToolStripButton searchButton = AwesomeFactory.newToolStripButton("search", "search");
		searchButton.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSearch();
			}
		});

		addButton(searchButton);
	}

	private MenuItem getWebContentMenuItem(GUIFolder folder, final GUIDocument document) {
		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		final MenuItem edit = new MenuItem(I18N.message("editdoc"));
		edit.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				if (document == null)
					return;

				if (document.getStatus() == 0) {
					// Need to checkout first
					DocumentService.Instance.get().checkout(new long[] { document.getId() }, new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

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
			}
		});

		final MenuItem create = new MenuItem(I18N.message("createdoc"));
		create.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				WebcontentCreate wcCreate = new WebcontentCreate();
				wcCreate.show();
			}
		});

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

		final MenuItem edit = new MenuItem(I18N.message("editdoc"));
		edit.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				if (document == null)
					return;

				if (document.getStatus() == 0) {
					// Need to checkout first
					DocumentService.Instance.get().checkout(new long[] { document.getId() }, new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

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
			}
		});

		final MenuItem create = new MenuItem(I18N.message("createdoc"));
		create.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				TextContentCreate tcCreate = new TextContentCreate();
				tcCreate.show();
			}
		});

		menu.setItems(edit, create);
		edit.setEnabled(document != null && document.getImmutable() == 0 && folder != null && folder.isDownload()
				&& folder.isWrite() && Util.isTextFile(document.getFileName()));
		create.setEnabled(
				folder != null && folder.isDownload() && folder.isWrite() && MainPanel.get().isOnDocumentsTab());

		MenuItem textcontentItems = new MenuItem(I18N.message("textcontent"));
		textcontentItems.setSubmenu(menu);

		return textcontentItems;
	}

	private MenuItem getDropboxMenuItem(GUIFolder folder, final GUIDocument document) {
		final MenuItem exportTo = new MenuItem(I18N.message("exporttodropbox"));
		exportTo.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				DropboxService.Instance.get().isConnected(new AsyncCallback<Boolean>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Boolean connected) {
						if (Boolean.FALSE.equals(connected))
							DropboxService.Instance.get().startAuthorization(new AsyncCallback<String>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(String authorizationUrl) {
									DropboxAuthorizationWizard wizard = new DropboxAuthorizationWizard(
											authorizationUrl);
									wizard.show();
								}
							});
						else {
							DropboxDialog dialog = new DropboxDialog(true);
							dialog.show();
						}
					}
				});
			}
		});

		final MenuItem importFrom = new MenuItem(I18N.message("importfromdropbox"));
		importFrom.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				DropboxService.Instance.get().isConnected(new AsyncCallback<Boolean>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Boolean connected) {
						if (Boolean.FALSE.equals(connected))
							DropboxService.Instance.get().startAuthorization(new AsyncCallback<String>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(String authorizationUrl) {
									DropboxAuthorizationWizard wizard = new DropboxAuthorizationWizard(
											authorizationUrl);
									wizard.show();
								}
							});
						else {
							DropboxDialog dialog = new DropboxDialog(false);
							dialog.show();
						}
					}
				});
			}
		});

		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);
		menu.setItems(exportTo, importFrom);

		exportTo.setEnabled(folder != null && folder.isDownload() && Feature.enabled(Feature.DROPBOX));
		importFrom.setEnabled(folder != null && folder.isWrite() && Feature.enabled(Feature.DROPBOX)
				&& MainPanel.get().isOnDocumentsTab());

		MenuItem dropboxItem = new MenuItem(I18N.message("dropbox"));
		dropboxItem.setSubmenu(menu);

		return dropboxItem;
	}

	private MenuItem getShareFileMenuItem(GUIFolder folder, final GUIDocument document) {
		final MenuItem exportTo = new MenuItem(I18N.message("exporttosharefile"));
		exportTo.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				ShareFileDialog dialog = new ShareFileDialog(true);
				dialog.show();
			}
		});

		final MenuItem importFrom = new MenuItem(I18N.message("importfromsharefile"));
		importFrom.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				ShareFileService.Instance.get().isAuthorized(new AsyncCallback<Boolean>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
						LD.clearPrompt();
					}

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
				});

			}
		});

		final MenuItem authorize = new MenuItem(I18N.message("authorize"));
		authorize.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				new ShareFileSettings().show();
			}
		});

		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);
		menu.setItems(exportTo, importFrom, authorize);

		exportTo.setEnabled(folder != null && folder.isDownload() && Feature.enabled(Feature.SHAREFILE));
		importFrom.setEnabled(folder != null && folder.isWrite() && Feature.enabled(Feature.SHAREFILE)
				&& MainPanel.get().isOnDocumentsTab());

		MenuItem sharefileItem = new MenuItem(I18N.message("sharefile"));
		sharefileItem.setSubmenu(menu);

		return sharefileItem;
	}

	private MenuItem getDocuSignMenuItem(GUIFolder folder, final GUIDocument document) {
		final MenuItem authorize = new MenuItem(I18N.message("authorize"));
		authorize.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				new DocuSignSettings().show();
			}
		});

		final MenuItem sendEnvelope = new MenuItem(I18N.message("sendenvelope"));
		sendEnvelope.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				LD.contactingServer();
				DocuSignService.Instance.get().isAuthorized(new AsyncCallback<Boolean>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
						LD.clearPrompt();
					}

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
			}
		});
		sendEnvelope.setEnabled(document != null);

		final MenuItem envelopes = new MenuItem(I18N.message("envelopes"));
		envelopes.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				LD.contactingServer();
				DocuSignService.Instance.get().isAuthorized(new AsyncCallback<Boolean>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
						LD.clearPrompt();
					}

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
			}
		});

		sendEnvelope.setEnabled(Feature.enabled(Feature.DOCUSIGN));
		envelopes.setEnabled(Feature.enabled(Feature.DOCUSIGN));
		authorize.setEnabled(Feature.enabled(Feature.DOCUSIGN));

		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);
		menu.setItems(sendEnvelope, envelopes, authorize);

		MenuItem docuSignItem = new MenuItem(I18N.message("docusign"));
		docuSignItem.setSubmenu(menu);

		return docuSignItem;
	}

	private MenuItem getOfficeMenuItem(GUIFolder folder, final GUIDocument document) {
		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		final MenuItem edit = new MenuItem(I18N.message("editwithoffice"));
		edit.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				if (document == null)
					return;

				WindowUtils.openUrl("ldedit:" + GWT.getHostPageBaseURL() + "ldedit?action=edit&sid="
						+ Session.get().getSid() + "&docId=" + document.getId());
			}
		});

		menu.setItems(edit);

		edit.setEnabled(document != null && document.getImmutable() == 0 && folder != null && folder.isDownload()
				&& folder.isWrite() && Feature.enabled(Feature.OFFICE));

		MenuItem officeItem = new MenuItem(I18N.message("microsoftoffice"));
		officeItem.setSubmenu(menu);

		return officeItem;
	}

	private void addToolsButton(GUIFolder folder, GUIDocument document) {
		tools = AwesomeFactory.newToolStripButton("toolbox", I18N.message("tools"), I18N.message("tools"));
		tools.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				Menu menu = buildToolsMenu(folder, document);
				menu.showContextMenu();
			}
		});

		addButton(tools, 2);
	}

	public Menu buildToolsMenu(GUIFolder folder, GUIDocument document) {
		Menu menu = new Menu();
		menu.setShowShadow(true);
		menu.setShadowDepth(3);

		if (folder == null && document != null)
			folder = document.getFolder();

		MenuItem develConsole = new MenuItem(I18N.message("develconsole"));
		develConsole.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				SC.showConsole();
			}
		});

		if (document != null || folder != null)
			addItemsWhenFolderOrDocumentSelected(folder, document, menu);

		menu.addItem(getSupportMenuItem());

		addRegistration(menu, develConsole);
		return menu;
	}

	private void addItemsWhenFolderOrDocumentSelected(GUIFolder folder, GUIDocument document, Menu menu) {
		if (Feature.enabled(Feature.DROPBOX)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.DROPBOX))
			menu.addItem(getDropboxMenuItem(folder, document));
		if (Feature.enabled(Feature.SHAREFILE)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.SHAREFILE))
			menu.addItem(getShareFileMenuItem(folder, document));
		if (Feature.enabled(Feature.GDRIVE)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.GDOCS))
			menu.addItem(new GDriveMenuItem(folder, document));
		if (Feature.enabled(Feature.ZOHO)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.ZOHO))
			menu.addItem(new ZohoMenuItem(folder, document));
		if (Feature.enabled(Feature.DOCUSIGN)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.DOCUSIGN))
			menu.addItem(getDocuSignMenuItem(folder, document));
		if (Feature.enabled(Feature.OFFICE)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.OFFICE))
			menu.addItem(getOfficeMenuItem(folder, document));
		if (Feature.enabled(Feature.WEBCONTENT)
				&& com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.WEBCONTENT))
			menu.addItem(getWebContentMenuItem(folder, document));
		if (com.logicaldoc.gui.common.client.Menu.enabled(com.logicaldoc.gui.common.client.Menu.TEXTCONTENT))
			menu.addItem(getTextContentMenuItem(folder, document));
	}

	private void addRegistration(Menu menu, MenuItem develConsole) {
		if (Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN)) {
			MenuItem registration = new MenuItem(I18N.message("registration"));
			registration.addClickHandler((MenuItemClickEvent registrationClick) -> {
				SettingService.Instance.get().loadSettingsByNames(
						new String[] { "reg.name", "reg.email", "reg.organization", "reg.website" },
						new AsyncCallback<GUIParameter[]>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIParameter[] reg) {
								String[] values = new String[reg.length];
								for (int j = 0; j < reg.length; j++) {
									values[j] = reg[j].getValue();
								}
								Registration r = new Registration(values);
								r.show();
							}
						});
			});
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
		density.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				String newDensity = event.getValue().toString();
				Session.get().getInfo().setConfig(Session.get().getTenantName() + ".gui.density", newDensity);
				CookiesManager.save(CookiesManager.COOKIE_DENSITY, newDensity);
				Util.setupDensity(Session.get().getInfo());
				Util.redirectToRoot();
			}
		});
		return density;
	}

	private void addSupportButton() {
		Menu menu = buildSupportMenu();

		ToolStripButton supportButton = AwesomeFactory.newToolStripButton("question-circle", "support");
		supportButton.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				menu.showContextMenu();
			}
		});

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
		manuals.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				Window.open(Session.get().getInfo().getBranding().getHelp() + "?lang=" + I18N.getLocale(), "_blank",
						"location=no,status=no,toolbar=no,menubar=no,resizable=yes,scrollbars=yes");
			}
		});
		menu.addItem(manuals);

		MenuItem bugReport = new MenuItem(I18N.message("bug.report"));
		bugReport.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				Window.open(Session.get().getInfo().getBranding().getBugs(), "_blank",
						"location=no,status=no,toolbar=no,menubar=no,resizable=yes,scrollbars=yes");
			}
		});
		if (Session.get().getInfo().getBranding().getBugs() != null
				&& !"-".equals(Session.get().getInfo().getBranding().getBugs()))
			menu.addItem(bugReport);

		MenuItem forum = new MenuItem(I18N.message("forum"));
		forum.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				Window.open(Session.get().getInfo().getBranding().getForum(), "_blank",
						"location=no,status=no,toolbar=no,menubar=no,resizable=yes,scrollbars=yes");
			}
		});
		if (Session.get().getInfo().getBranding().getForum() != null
				&& !"-".equals(Session.get().getInfo().getBranding().getForum()))
			menu.addItem(forum);

		MenuItem about = new MenuItem(I18N.message("about") + " " + Session.get().getInfo().getBranding().getProduct());
		about.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(MenuItemClickEvent event) {
				AboutDialog dialog = new AboutDialog();
				dialog.show();
			}
		});
		menu.addItem(about);
		return menu;
	}

	private void addLogoutButton() {
		ToolStripButton logoutButton = AwesomeFactory.newToolStripButton("power-off", "logout");
		logoutButton.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				Session.get().logout();
			}
		});
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
	public void onDocumentsDeleted(GUIDocument[] documents) {
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
}