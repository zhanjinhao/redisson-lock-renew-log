package cn.addenda.redissonlock.renewlog;

import lombok.extern.slf4j.Slf4j;
import org.redisson.Redisson;
import org.redisson.RedissonFairLock;
import org.redisson.RedissonLock;
import org.redisson.RedissonReadWriteLock;
import org.redisson.api.RedissonClient;
import org.redisson.api.options.CommonOptions;
import org.redisson.api.options.CommonParams;
import org.redisson.command.CommandAsyncExecutor;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

@Slf4j
public class RenewLogInvocationHandler1 implements InvocationHandler {

  private final RedissonClient redissonClient;

  /**
   * true： 只能调用包装后的方法，调用非包装的方法抛异常。
   * false：可以调用非包装的方法。调用非包装的方法时和调用原始的方法一样。
   */
  private final boolean forceInvokeWrap;

  public RenewLogInvocationHandler1(RedissonClient redissonClient, boolean forceInvokeWrap) {
    this.redissonClient = redissonClient;
    this.forceInvokeWrap = forceInvokeWrap;
    if (log.isInfoEnabled()) {
      log.info("A proxy of RedissonClient[{}] has been created. The field `forceInvokeWrap` is set to [{}]", redissonClient, forceInvokeWrap);
    }
  }

  @Override
  public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
    String methodName = method.getName();
    if ("getFairLock".equals(methodName) && args.length == 1 && args[0] != null
            && CommonOptions.class.isAssignableFrom(args[0].getClass())) {
      CommandAsyncExecutor commandExecutor = ((Redisson) redissonClient).getCommandExecutor();
      CommonParams params = (CommonParams) args[0];
      String name = params.getName();
      log(RenewLogRedissonFairLock.class, RedissonFairLock.class, name);
      return new RenewLogRedissonFairLock(commandExecutor.copy(params), params.getName());
    }
    if ("getFairLock".equals(methodName) && args.length == 1 && args[0] != null
            && String.class.isAssignableFrom(args[0].getClass())) {
      CommandAsyncExecutor commandExecutor = ((Redisson) redissonClient).getCommandExecutor();
      String name = (String) args[0];
      log(RenewLogRedissonFairLock.class, RedissonFairLock.class, name);
      return new RenewLogRedissonFairLock(commandExecutor, (String) args[0]);
    }
    if ("getLock".equals(methodName) && args.length == 1 && args[0] != null
            && CommonOptions.class.isAssignableFrom(args[0].getClass())) {
      CommandAsyncExecutor commandExecutor = ((Redisson) redissonClient).getCommandExecutor();
      CommonParams params = (CommonParams) args[0];
      String name = params.getName();
      log(RenewLogRedissonLock.class, RedissonLock.class, name);
      return new RenewLogRedissonLock(commandExecutor.copy(params), name);
    }
    if ("getLock".equals(methodName) && args.length == 1 && args[0] != null
            && String.class.isAssignableFrom(args[0].getClass())) {
      String name = (String) args[0];
      CommandAsyncExecutor commandExecutor = ((Redisson) redissonClient).getCommandExecutor();
      log(RenewLogRedissonLock.class, RedissonLock.class, name);
      return new RenewLogRedissonLock(commandExecutor, name);
    }
    if ("getReadWriteLock".equals(methodName) && args.length == 1 && args[0] != null
            && CommonOptions.class.isAssignableFrom(args[0].getClass())) {
      CommandAsyncExecutor commandExecutor = ((Redisson) redissonClient).getCommandExecutor();
      CommonParams params = (CommonParams) args[0];
      String name = params.getName();
      log(RenewLogRedissonReadWriteLock.class, RedissonReadWriteLock.class, name);
      return new RenewLogRedissonReadWriteLock(commandExecutor.copy(params), name);
    }
    if ("getReadWriteLock".equals(methodName) && args.length == 1 && args[0] != null
            && String.class.isAssignableFrom(args[0].getClass())) {
      CommandAsyncExecutor commandExecutor = ((Redisson) redissonClient).getCommandExecutor();
      String name = (String) args[0];
      log(RenewLogRedissonReadWriteLock.class, RedissonReadWriteLock.class, name);
      return new RenewLogRedissonReadWriteLock(commandExecutor, name);
    }
    if (!forceInvokeWrap) {
      return method.invoke(redissonClient, args);
    }
    throw new UnsupportedOperationException("RenewLogRedissonClient only support the following methods: getLock、getFairLock、getReadWriteLock, since the `forceInvokeWrap` field is set to [true].");
  }

  private void log(Class<?> clazz, Class<?> originalClazz, String name) {
    if (log.isDebugEnabled()) {
      log.debug("Return an object of {}[{}] instead of {}, and the return will log error msg when renewing ttl.",
              clazz.getSimpleName(), name, originalClazz.getSimpleName());
    }
  }

}
