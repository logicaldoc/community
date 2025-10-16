package com.logicaldoc.core.security.apikey;

import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.List;
import java.util.Map;

import jakarta.transaction.Transactional;
import jakarta.xml.bind.DatatypeConverter;

import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

import com.logicaldoc.core.HibernatePersistentObjectDAO;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.crypt.CryptUtil;
import com.logicaldoc.util.html.HTMLSanitizer;

/**
 * Hibernate implementation of {@link ApiKeyDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 */
@Repository("apiKeyDAO")
@Transactional
public class HibernateApiKeyDAO extends HibernatePersistentObjectDAO<ApiKey> implements ApiKeyDAO {

	private HibernateApiKeyDAO() {
		super(ApiKey.class);
		super.log = LoggerFactory.getLogger(HibernateApiKeyDAO.class);
	}

	@Override
	public void store(ApiKey apiKey) throws PersistenceException {
		if (apiKey.getId() == 0L)
			try {
				apiKey.setDecodedKey(generateKey(256));
			} catch (NoSuchAlgorithmException e) {
				throw new PersistenceException(e.getMessage(), e);
			}
		apiKey.setName(HTMLSanitizer.sanitizeSimpleText(apiKey.getName()));
		super.store(apiKey);
	}

	@Override
	public ApiKey findByName(String name, long userId) throws PersistenceException {
		return findByWhere("_entity.name = :name and _entity.userId = :userId", Map.of("name", name, "userId", userId),
				null, null).stream().findFirst().orElse(null);
	}

	@Override
	public ApiKey findByKey(String key) throws PersistenceException, NoSuchAlgorithmException {
		return findByWhere("_entity.key = :key", Map.of("key", CryptUtil.encryptSHA256(key)), null, null).stream()
				.findFirst().orElse(null);
	}

	@Override
	public List<ApiKey> findByUser(long userId) throws PersistenceException {
		return findByWhere("_entity.userId = :userId", Map.of("userId", userId), "_entity.name asc", null);
	}

	@Override
	public void delete(long id, int code) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		ApiKey apiKey = findById(id);
		apiKey.setDeleted(code);
		apiKey.setName(apiKey.getName() + "." + apiKey.getId());

		saveOrUpdate(apiKey);
	}

	private String generateKey(int keyLen) {
		SecureRandom random = new SecureRandom();
		byte[] bytes = new byte[keyLen / 8];
		random.nextBytes(bytes);
		return "ld-" + DatatypeConverter.printHexBinary(bytes).toLowerCase();
	}
}