package ahc.dms.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.orm.jpa.JpaProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.env.Environment;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import javax.sql.DataSource;
import java.util.HashMap;

// Configuration for PostgreSQL edms
@Configuration
@PropertySource({ "classpath:persistence.properties" })
@EnableTransactionManagement
@EnableJpaRepositories(
        basePackages = "ahc.dms.dao.pgdms.repositories",
        entityManagerFactoryRef = "pgDmsEntityManager",
        transactionManagerRef = "pgDmsTransactionManager"
)
public class PgDmsConfig {

    @Autowired
    private Environment env;

    @Bean(name = "pgDmsDataSource")
    //@ConfigurationProperties(prefix = "spring.datasource.pg_dms")
    public DataSource pgDmsDataSource() {
        DriverManagerDataSource dataSource = new DriverManagerDataSource();
        dataSource.setDriverClassName(env.getProperty("spring.datasource.pg_dms.driverClassName"));
        dataSource.setUrl(env.getProperty("spring.datasource.pg_dms.url"));
        dataSource.setUsername(env.getProperty("spring.datasource.pg_dms.username"));
        dataSource.setPassword(env.getProperty("spring.datasource.pg_dms.password"));

        return dataSource;
    }

    @Bean(name = "pgDmsEntityManager")
    //@ConfigurationProperties(prefix = "spring.jpa.pg_dms.properties")
    public LocalContainerEntityManagerFactoryBean pgDmsEntityManager() {
        LocalContainerEntityManagerFactoryBean em = new LocalContainerEntityManagerFactoryBean();
        em.setDataSource(pgDmsDataSource());
        em.setPackagesToScan(new String[] { "ahc.dms.dao.pgdms.entities" });

        HibernateJpaVendorAdapter vendorAdapter = new HibernateJpaVendorAdapter();
        em.setJpaVendorAdapter(vendorAdapter);

        HashMap<String, Object> properties = new HashMap<>();
        properties.put("hibernate.hbm2ddl.auto", env.getProperty("spring.jpa.pg_dms.properties.hibernate.hbm2ddl.auto"));
        properties.put("hibernate.dialect", env.getProperty("spring.jpa.pg_dms.properties.hibernate.dialect"));
        properties.put("hibernate.show_sql", "spring.jpa.pg_dms.properties.show-sql");
        properties.put("hibernate.format_sql", "spring.jpa.pg_dms.properties.format-sql");
        em.setJpaPropertyMap(properties);

        return em;
    }

    @Bean(name = "pgDmsTransactionManager")
    public PlatformTransactionManager pgDmsTransactionManager() {

        JpaTransactionManager transactionManager = new JpaTransactionManager();
        transactionManager.setEntityManagerFactory(pgDmsEntityManager().getObject());
        return transactionManager;
    }
}