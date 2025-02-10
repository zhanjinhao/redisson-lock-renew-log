package cn.addenda.redissonlock.renewlog;

import lombok.SneakyThrows;
import org.junit.Test;
import org.redisson.Redisson;
import org.redisson.api.RLock;
import org.redisson.api.RReadWriteLock;
import org.redisson.api.RedissonClient;
import org.redisson.config.Config;

import java.io.InputStream;
import java.util.Properties;

public class TestRedissonLock {

  @SneakyThrows
  public RedissonClient redissonClient() {
    // 写一段代码读取test/resources/db.properties文件，用properties接收
    Properties properties = new Properties();
    try (InputStream input = TestRedissonLock.class.getClassLoader().getResourceAsStream("db.properties")) {
      properties.load(input);
    }

    // 配置
    Config config = new Config();
    config.useSingleServer()
            .setConnectionPoolSize(1)
            .setConnectionMinimumIdleSize(1)
            .setSubscriptionConnectionPoolSize(1)
            .setSubscriptionConnectionMinimumIdleSize(1)
            .setAddress(properties.getProperty("address"))
            .setPassword(properties.getProperty("password"));
    // 创建RedissonClient对象
    return Redisson.create(config);
  }

  @Test
  public void test1() {
    RedissonClient redissonClient = redissonClient();
    redissonClient = RedissonLockRenewLogWrapper.wrap(redissonClient);
    RLock lock = redissonClient.getLock("123");
    lock.lock();
    try {
      Thread.sleep(100000);
    } catch (InterruptedException e) {
      throw new RuntimeException(e);
    } finally {
      lock.unlock();
    }
  }

  @Test
  public void test2() {
    RedissonClient redissonClient = redissonClient();
    redissonClient = RedissonLockRenewLogWrapper.wrap(redissonClient);
    RLock lock = redissonClient.getFairLock("123");
    lock.lock();
    try {
      Thread.sleep(100000);
    } catch (InterruptedException e) {
      throw new RuntimeException(e);
    } finally {
      lock.unlock();
    }
  }

  @Test
  public void test3() {
    RedissonClient redissonClient = redissonClient();
    redissonClient = RedissonLockRenewLogWrapper.wrap(redissonClient);
    RReadWriteLock lock = redissonClient.getReadWriteLock("123");
    RLock rLock = lock.writeLock();
    rLock.lock();
    try {
      Thread.sleep(100000);
    } catch (InterruptedException e) {
      throw new RuntimeException(e);
    } finally {
      rLock.unlock();
    }
  }

  @Test
  public void test4() {
    RedissonClient redissonClient = redissonClient();
    redissonClient = RedissonLockRenewLogWrapper.wrap(redissonClient);
    RReadWriteLock lock = redissonClient.getReadWriteLock("123");
    RLock rLock = lock.readLock();
    rLock.lock();
    try {
      Thread.sleep(100000);
    } catch (InterruptedException e) {
      throw new RuntimeException(e);
    } finally {
      rLock.unlock();
    }
  }

}
