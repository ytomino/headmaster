#include <objc/NxConstStr.h>
#include <objc/Protocol.h>

@protocol pr
- (void)print: (NXConstantString *)message;
@end

@interface t: Object <pr>
{
}
@end

@implementation t
- (void)print: (NXConstantString *)message
{
	printf("%.*s, world!\n", [message length], [message cString]);
}
@end

int main()
{
	t<pr> *obj = [[t alloc] init];
	id<pr> p = obj;
	[p print: @"Hello"];
	[obj free];
	return 0;
}
