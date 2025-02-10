package cn.addenda.redissonlock.renewlog;

import lombok.extern.slf4j.Slf4j;
import org.redisson.RedissonLock;
import org.redisson.command.CommandAsyncExecutor;

import java.util.concurrent.CompletionStage;
import java.util.function.BiConsumer;

@Slf4j
public class RenewLogRedissonLock extends RedissonLock {

  public RenewLogRedissonLock(CommandAsyncExecutor commandExecutor, String name) {
    super(commandExecutor, name);
  }

  public RenewLogRedissonLock(String name, CommandAsyncExecutor commandExecutor) {
    super(name, commandExecutor);
  }

  @Override
  protected CompletionStage<Boolean> renewExpirationAsync(long threadId) {
    CompletionStage<Boolean> stage = super.renewExpirationAsync(threadId);
    return stage
            .whenComplete(new BiConsumer<Boolean, Throwable>() {
              @Override
              public void accept(Boolean aBoolean, Throwable throwable) {
                if (throwable != null) {
                  log.error("[{}] failed to renew RedissonLock [{}], and a throwable is been thrown.", getLockName(threadId), getRawName(), throwable);
                  return;
                }
                if (Boolean.TRUE.equals(aBoolean)) {
                  log.error("[{}] succeed in renewing RedissonLock [{}].", getLockName(threadId), getRawName());
                  return;
                }
                if (Boolean.FALSE.equals(aBoolean)) {
                  log.error("[{}] failed to renew RedissonLock [{}].", getLockName(threadId), getRawName());
                  return;
                }
              }
            });
  }

}
