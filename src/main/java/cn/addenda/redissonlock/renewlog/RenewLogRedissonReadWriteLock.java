package cn.addenda.redissonlock.renewlog;

import org.redisson.RedissonReadWriteLock;
import org.redisson.api.RLock;
import org.redisson.command.CommandAsyncExecutor;

public class RenewLogRedissonReadWriteLock extends RedissonReadWriteLock {

  public RenewLogRedissonReadWriteLock(CommandAsyncExecutor commandExecutor, String name) {
    super(commandExecutor, name);
  }

  @Override
  public RLock readLock() {
    return new RenewLogRedissonReadLock(commandExecutor, getName());
  }

  @Override
  public RLock writeLock() {
    return new RenewLogRedissonWriteLock(commandExecutor, getName());
  }

}
